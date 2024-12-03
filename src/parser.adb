with Utils;    use Utils;
with Geometry; use Geometry;
with Colour;
with Primitives;
with Materials;

with Ada.Numerics;          use Ada.Numerics;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with JSON.Parsers;
with JSON.Types;
with Ada.Containers.Vectors;

package body Parser is
   type Mat_Mapping is record
      Key : Unbounded_String;
   end record;
   type Mapping is array (Scene.Material_Index'Range) of Mat_Mapping;

   function Find_Material
     (Name : String; Map : Mapping) return Scene.Material_Index is
   begin
      for I in Map'Range loop
         if Map (I).Key = Name then
            return I;
         end if;
      end loop;
      Put_Line ("no such material: '" & Name & "'");
      raise Program_Error;
   end Find_Material;

   function "=" (A, B : Scene.Object) return Boolean is
   begin
      return False;
   end "=";
   package World is new Ada.Containers.Vectors (Natural, Scene.Object);
   package JTypes is new JSON.Types (Integer, Real);
   package JParsers is new JSON.Parsers (JTypes);

   procedure Parse_Point (F : in out File_Type; P : out Point) is
      X, Y, Z : Real;
   begin
      Real_IO.Get (F, X);
      Real_IO.Get (F, Y);
      Real_IO.Get (F, Z);
      P := (X, Y, Z);
   end Parse_Point;

   procedure Parse_Tri_N (F : in out File_Type; T : out Primitives.Triangle_N)
   is
      O, A, B    : Point;
      NO, NA, NB : Vec3;
   begin
      Parse_Point (F, O);
      Parse_Point (F, NO);
      Parse_Point (F, A);
      Parse_Point (F, NA);
      Parse_Point (F, B);
      Parse_Point (F, NB);
      T := Primitives.Create (O, A, B, NO, NA, NB);
   end Parse_Tri_N;

   procedure Parse_N_Triangles
     (F : in out File_Type;
      N : Natural;
      M : Scene.Material_Index;
      W : in out World.Vector)
   is
      T : Primitives.Triangle_N;
   begin
      for I in 1 .. N loop
         Parse_Tri_N (F, T);
         W.Append (Scene.Object'(Scene.Triangle_N, M, T));
      end loop;
   end Parse_N_Triangles;

   procedure Parse_Triangles
     (Filename : String; M : Scene.Material_Index; W : in out World.Vector)
   is
      F : File_Type;
      N : Natural;
   begin
      Open (F, In_File, Filename);
      Ada.Integer_Text_IO.Get (F, N);
      Parse_N_Triangles (F, N, M, W);
      Close (F);
   end Parse_Triangles;

   function Vec_To_Array (Vec : World.Vector) return Scene.World is
      W : Scene.World :=
        new Scene.World_Array (Vec.First_Index .. Vec.Last_Index);
   begin
      for I in Vec.First_Index .. Vec.Last_Index loop
         W (I) := Vec (I);
      end loop;
      return W;
   end Vec_To_Array;

   function Get_Vec3 (Value : JTypes.JSON_Value) return Vec3 is
      V : Vec3;
   begin
      V.X := Value.Get (1).Value;
      V.Y := Value.Get (2).Value;
      V.Z := Value.Get (3).Value;
      return V;
   end Get_Vec3;

   procedure Create_Camera
     (Value : JTypes.JSON_Value;
      Cam   : out Camera.Camera;
      AR    : out Camera.Aspect_Ratio) is
   begin
      AR :=
        (Value.Get ("aspect_ratio").Get (1).Value,
         Value.Get ("aspect_ratio").Get (2).Value);
      Cam :=
        Camera.Create
          (Get_Vec3 (Value.Get ("origin")),
           Get_Vec3 (Value.Get ("look_at")),
           AR,
           Value.Get ("vfov").Value,
           Get_Vec3 (Value.Get ("up")));
   end Create_Camera;

   function Get_Rgb (Value : JTypes.JSON_Value) return Colour.Rgb is
      S       : String := Value.Value;
      R, G, B : Natural;
      function Hex (C : Character) return Natural is
      begin
         case C is
            when '0' .. '9' =>
               return Character'Pos (C) - Character'Pos ('0');

            when 'a' .. 'f' =>
               return Character'Pos (C) - Character'Pos ('a') + 10;

            when 'A' .. 'F' =>
               return Character'Pos (C) - Character'Pos ('A') + 10;

            when others =>
               raise Program_Error;
         end case;
      end Hex;
   begin
      if S'Length /= 7 or else S (S'First) /= '#' then
         raise Program_Error;
      end if;
      R := Hex (S (S'First + 1)) * 16 + Hex (S (S'First + 2));
      G := Hex (S (S'First + 3)) * 16 + Hex (S (S'First + 4));
      B := Hex (S (S'First + 5)) * 16 + Hex (S (S'First + 6));
      return
        ((Real (R) / 255.0) ** 2,
         (Real (G) / 255.0) ** 2,
         (Real (B) / 255.0) ** 2);
   end Get_Rgb;

   procedure Create_Materials
     (Value : JTypes.JSON_Value;
      Mat   : out Scene.Material_Map;
      Map   : out Mapping)
   is
      Idx : Scene.Material_Index := 0;
      K   : Unbounded_String;
      C   : Colour.Rgb;

      use Scene;
   begin
      case Value.Kind is
         when JTypes.Object_Kind =>
            for E of Value loop
               Map (Idx) := (Key => To_Unbounded_String (String'(E.Value)));
               if Length (Map (Idx).Key) = 0 then
                  Put_Line ("material name cannot be empty");
                  raise Program_Error;
               end if;
               declare
                  V : JTypes.JSON_Value := Value.Get (String'(E.Value));
                  K : String := V.Get ("kind").Value;
               begin
                  C := Get_Rgb (V.Get ("colour"));

                  if K = "diffuse" then
                     Mat (Idx) := (Materials.Diffuse, C);
                  elsif K = "specular" then
                     Mat (Idx) :=
                       (Materials.Specular, C, V.Get ("roughness").Value);
                  elsif K = "dielectric" then
                     Mat (Idx) :=
                       (Materials.Dielectric,
                        C,
                        V.Get ("index").Value,
                        V.Get ("fuzziness").Value);
                  elsif K = "plastic" then
                     Mat (Idx) :=
                       (Materials.Plastic,
                        C,
                        V.Get ("index").Value,
                        V.Get ("fuzziness").Value);
                  elsif K = "emissive" then
                     Mat (Idx) :=
                       (Materials.Emissive, C, V.Get ("intensity").Value);
                  else
                     Put_Line ("unknown material kind: '" & K & "'");
                     raise Program_Error;
                  end if;

               end;
               Idx := Idx + 1;
            end loop;

         when others =>
            raise Program_Error;
      end case;
   end Create_Materials;

   procedure Create_Objects
     (Value : JTypes.JSON_Value; W : out Scene.World; Map : Mapping)
   is
      W_Builder : World.Vector;
   begin
      case Value.Kind is
         when JTypes.Array_Kind =>
            for O of Value loop
               declare
                  K                     : String := O.Get ("kind").Value;
                  M                     : Scene.Material_Index :=
                    Find_Material (O.Get ("material").Value, Map);
                  Idx                   : Natural :=
                    Natural (W_Builder.Length);
                  Rotation, Translation : Vec3;
                  Scale                 : Real;
                  Obj                   : Scene.Object;
                  Transforms            : Boolean := False;
               begin
                  if K = "sphere" then
                     W_Builder.Append
                       (Scene.Object'
                          (Scene.Sphere,
                           M,

                             (Get_Vec3 (O.Get ("center")),
                              O.Get ("radius").Value)));
                  elsif K = ".norm" then
                     Parse_Triangles (O.Get ("filename").Value, M, W_Builder);
                  else
                     raise Program_Error;
                  end if;

                  if O.Contains ("rotate") then
                     Rotation := Get_Vec3 (O.Get ("rotate")) * (2.0 * Pi);
                     Transforms := True;
                  end if;
                  if O.Contains ("scale") then
                     Scale := O.Get ("scale").Value;
                     Transforms := True;
                  end if;
                  if O.Contains ("translate") then
                     Translation := Get_Vec3 (O.Get ("translate"));
                     Transforms := True;
                  end if;
                  if Transforms then
                     for I in Idx .. W_Builder.Last_Index loop
                        Obj := W_Builder (I);
                        case Obj.Kind is
                           when Scene.Sphere =>
                              Obj.S.Radius := Obj.S.Radius * Scale;
                              Obj.S.Origin := Obj.S.Origin + Translation;

                           when Scene.Triangle_N =>
                              Obj.TN.Origin :=
                                Rotate (Obj.TN.Origin, Rotation) * Scale
                                + Translation;
                              Obj.TN.V1 :=
                                Rotate (Obj.TN.V1, Rotation) * Scale;
                              Obj.TN.V2 :=
                                Rotate (Obj.TN.V2, Rotation) * Scale;
                              Obj.TN.N :=
                                Rotate (Obj.TN.N, Rotation) * (Scale * Scale);
                              Obj.TN.N0 := Rotate (Obj.TN.N0, Rotation);
                              Obj.TN.N1 := Rotate (Obj.TN.N1, Rotation);
                              Obj.TN.N2 := Rotate (Obj.TN.N2, Rotation);

                           when Scene.Triangle =>
                              Obj.T.Origin :=
                                Rotate (Obj.T.Origin, Rotation) * Scale
                                + Translation;
                              Obj.T.V1 := Rotate (Obj.T.V1, Rotation) * Scale;
                              Obj.T.V2 := Rotate (Obj.T.V2, Rotation) * Scale;
                              Obj.T.N :=
                                Rotate (Obj.T.N, Rotation) * (Scale * Scale);
                        end case;
                        W_Builder.Replace_Element (I, Obj);
                     end loop;
                  end if;
               end;
            end loop;
            W := Vec_To_Array (W_Builder);

         when others =>
            raise Program_Error;
      end case;
   end Create_Objects;

   function Parse_Scene_File
     (Filename             : String;
      Mat                  : out Scene.Material_Map;
      Cam                  : out Camera.Camera;
      Img                  : out Image.Image;
      Samples, Max_Bounces : out Positive;
      Background           : out Colour.Rgb) return Scene.World
   is
      W      : Scene.World;
      Parser : JParsers.Parser := JParsers.Create_From_File (Filename);
      Value  : constant JTypes.JSON_Value := Parser.Parse;
      Height : Positive;
      AR     : Camera.Aspect_Ratio;
      Map    : Mapping;
   begin
      Create_Camera (Value.Get ("camera"), Cam, AR);
      Height := Value.Get ("image").Get ("height").Value;
      Samples := Value.Get ("image").Get ("samples").Value;
      Max_Bounces := Value.Get ("image").Get ("max_bounces").Value;
      Background := Get_Rgb (Value.Get ("image").Get ("background"));
      Img := Image.Create (Camera.Height_To_Width (AR, Height), Height);
      Create_Materials (Value.Get ("materials"), Mat, Map);
      Create_Objects (Value.Get ("objects"), W, Map);
      return W;
   end Parse_Scene_File;

end Parser;
