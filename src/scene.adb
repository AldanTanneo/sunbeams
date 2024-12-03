with Ada.Calendar;   use Ada.Calendar;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Scene is
   pragma Suppress_All;

   function Intersect
     (Self : Object; R : Ray; Min, Max : Real) return Primitives.Hit is
   begin
      case Self.Kind is
         when Sphere =>
            return Primitives.Intersect (Self.S, R, Min, Max);

         when Triangle =>
            return Primitives.Intersect (Self.T, R, Min, Max);

         when Triangle_N =>
            return Primitives.Intersect (Self.TN, R, Min, Max);
      end case;
   end Intersect;

   function Intersect (Box : Aabb; R : Ray; Min, Max : Real) return Real is
      TX1, TX2, TY1, TY2, TZ1, TZ2, TMin, TMax : Real;
   begin
      TX1 := (Box.Min.X - R.Origin.X) / R.Direction.X;
      TX2 := (Box.Max.X - R.Origin.X) / R.Direction.X;

      TMin := Real'Min (TX1, TX2);
      TMax := Real'Max (TX1, TX2);

      TY1 := (Box.Min.Y - R.Origin.Y) / R.Direction.Y;
      TY2 := (Box.Max.Y - R.Origin.Y) / R.Direction.Y;

      TMin := Real'Max (TMin, Real'Min (TY1, TY2));
      TMax := Real'Min (TMax, Real'Max (TY1, TY2));

      TZ1 := (Box.Min.Z - R.Origin.Z) / R.Direction.Z;
      TZ2 := (Box.Max.Z - R.Origin.Z) / R.Direction.Z;

      TMin := Real'Max (TMin, Real'Min (TZ1, TZ2));
      TMax := Real'Min (TMax, Real'Max (TZ1, TZ2));

      if TMax >= TMin and then TMax >= Min and then TMin <= Max then
         return Real'Max (Min, TMin);
      else
         return -1.0;
      end if;
   end Intersect;

   function Intersect
     (Self      : World;
      Hierarchy : BVH;
      R         : Ray;
      Min, Max  : Real;
      Node      : BVH_Node := 0) return World_Hit
   is
      use Primitives;
      N1, N2, NTmp : BVH_Node;
      T1, T2, TTmp : Real := -1.0;
      H            : Hit;
      Rec, Tmp     : World_Hit;
      TMax         : Real := Max;
   begin
      Rec.H.Hit := None;

      if Hierarchy (Node).Left /= 0 then
         N1 := Hierarchy (Node).Left;
         N2 := Hierarchy (Node).Right;
         T1 := Intersect (Hierarchy (N1).Bounds, R, Min, Max);
         T2 := Intersect (Hierarchy (N2).Bounds, R, Min, Max);

         if T2 < T1 then
            NTmp := N1;
            TTmp := T1;
            N1 := N2;
            T1 := T2;
            N2 := NTmp;
            T2 := TTmp;
         end if;

         if T1 >= Min then
            Tmp := Intersect (Self, Hierarchy, R, Min, TMax, N1);
            if Tmp.H.Hit /= None then
               Rec := Tmp;
               TMax := Tmp.H.Time;
            end if;
         end if;

         if T2 >= Min then
            Tmp := Intersect (Self, Hierarchy, R, Min, TMax, N2);
            if Tmp.H.Hit /= None then
               Rec := Tmp;
               TMax := Tmp.H.Time;
            end if;
         end if;
      else
         for I in Hierarchy (Node).First .. Hierarchy (Node).Last loop
            H := Intersect (Self (I), R, Min, TMax);
            if H.Hit /= None then
               Rec.H := H;
               Rec.M := Self (I).Material;
               TMax := H.Time;
            end if;
         end loop;
      end if;

      return Rec;
   end Intersect;

   function Area (Box : Aabb) return Real is
      Extent : constant Vec3 := Box.Max - Box.Min;
   begin
      return Extent.X * Extent.Y + Extent.Y * Extent.Z + Extent.Z * Extent.X;
   end Area;

   procedure Grow (Box : in out Aabb; Obj : Object) is
   begin
      case Obj.Kind is
         when Triangle =>
            Box.Min := Min (Box.Min, Obj.T.Origin);
            Box.Min := Min (Box.Min, Obj.T.Origin + Obj.T.V1);
            Box.Min := Min (Box.Min, Obj.T.Origin + Obj.T.V2);

            Box.Max := Max (Box.Max, Obj.T.Origin);
            Box.Max := Max (Box.Max, Obj.T.Origin + Obj.T.V1);
            Box.Max := Max (Box.Max, Obj.T.Origin + Obj.T.V2);

         when Triangle_N =>
            Box.Min := Min (Box.Min, Obj.TN.Origin);
            Box.Min := Min (Box.Min, Obj.TN.Origin + Obj.TN.V1);
            Box.Min := Min (Box.Min, Obj.TN.Origin + Obj.TN.V2);

            Box.Max := Max (Box.Max, Obj.TN.Origin);
            Box.Max := Max (Box.Max, Obj.TN.Origin + Obj.TN.V1);
            Box.Max := Max (Box.Max, Obj.TN.Origin + Obj.TN.V2);

         when Sphere =>
            Box.Min := Min (Box.Min, Obj.S.Origin - Obj.S.Radius * ONES);
            Box.Min := Min (Box.Min, Obj.S.Origin + Obj.S.Radius * ONES);

            Box.Max := Max (Box.Max, Obj.S.Origin - Obj.S.Radius * ONES);
            Box.Max := Max (Box.Max, Obj.S.Origin + Obj.S.Radius * ONES);
      end case;
   end Grow;

   function Build_BVH (W : World) return BVH is
      package BVH_Vector is new
        Ada.Containers.Vectors (Index_Type => BVH_Node, Element_Type => Box);
      use BVH_Vector;

      H : Vector;
      -- H : BVH (0 .. (2 * W'Length - 2));
      type Point_Array is array (Natural range <>) of Point;
      type Centroids is access Point_Array;
      procedure Point_Array_Free is new
        Ada.Unchecked_Deallocation (Point_Array, Centroids);
      C : Centroids := new Point_Array (W'First .. W'Last);

      function Vec_To_Array (Vec : Vector) return BVH is
         H : BVH := new BVH_Array (Vec.First_Index .. Vec.Last_Index);
      begin
         for I in Vec.First_Index .. Vec.Last_Index loop
            H (I) := Vec (I);
         end loop;
         return H;
      end Vec_To_Array;

      procedure Update_Node_Bounds (Node : BVH_Node) is
         Box : Aabb;
      begin
         for I in H (Node).First .. H (Node).Last loop
            Grow (Box, W (I));
         end loop;
         H (Node).Bounds.Min := Box.Min;
         H (Node).Bounds.Max := Box.Max;
      end Update_Node_Bounds;

      function Surface_Area_Heuristic
        (Node : BVH_Node; A : Axis; Pos : Real) return Real
      is
         Cost             : Real;
         L_Box, R_Box     : Aabb;
         L_Count, R_Count : Natural := 0;
      begin
         for I in H (Node).First .. H (Node).Last loop
            if Get (C (I), A) < Pos then
               L_Count := L_Count + 1;
               Grow (L_Box, W (I));
            else
               R_Count := R_Count + 1;
               Grow (R_Box, W (I));
            end if;
         end loop;
         Cost := Real (L_Count) * Area (L_Box) + Real (R_Count) * Area (R_Box);
         if Cost > 0.0 then
            return Cost;
         else
            return Real'Last;
         end if;
      end Surface_Area_Heuristic;

      function Find_Split_Plane
        (Node : BVH_Node; Ax : out Axis; Split_Pos : out Real) return Real
      is
         Sweep_Count                    : constant := 100;
         Best_Cost                      : Real := Real'Last;
         Cost, Pos, B_Min, B_Max, Scale : Real;
      begin
         for A in Axis'Range loop
            B_Min := Get (H (Node).Bounds.Min, A);
            B_Max := Get (H (Node).Bounds.Max, A);
            if (B_Min < B_Max) then
               Scale := (B_Max - B_Min) / Real (Sweep_Count);
               for I in 1 .. Sweep_Count - 1 loop
                  Pos := B_Min + Real (I) * Scale;
                  Cost := Surface_Area_Heuristic (Node, A, Pos);
                  if Cost < Best_Cost then
                     Ax := A;
                     Split_Pos := Pos;
                     Best_Cost := Cost;
                  end if;
               end loop;
            end if;
         end loop;
         return Best_Cost;
      end Find_Split_Plane;

      B : Box;

      procedure Subdivide (Node : BVH_Node) is
         A                : Axis;
         Split            : Real;
         Parent_Cost      : Real :=
           Real (H (Node).Last - H (Node).First + 1) * Area (H (Node).Bounds);
         Cost             : Real := Find_Split_Plane (Node, A, Split);
         C_Tmp            : Point;
         Obj_Tmp          : Object;
         I                : Natural := H (Node).First;
         J                : Natural := H (Node).Last;
         L_Child, R_Child : BVH_Node;
      begin
         if Cost >= Parent_Cost then
            H (Node).Left := 0;
            H (Node).Right := 0;
         else
            while I < J loop
               if Get (C (I), A) < Split then
                  I := I + 1;
               else
                  C_Tmp := C (I);
                  Obj_Tmp := W (I);
                  C (I) := C (J);
                  W (I) := W (J);
                  C (J) := C_Tmp;
                  W (J) := Obj_Tmp;
                  J := J - 1;
               end if;
            end loop;
            if I = H (Node).First or else J = H (Node).Last then
               H (Node).Left := 0;
               H (Node).Right := 0;
            else
               H.Append (B);
               L_Child := H.Last_Index;
               H.Append (B);
               R_Child := H.Last_Index;

               H (Node).Left := L_Child;
               H (Node).Right := R_Child;
               H (L_Child).First := H (Node).First;
               H (L_Child).Last := I - 1;
               H (R_Child).First := I;
               H (R_Child).Last := H (Node).Last;

               Update_Node_Bounds (L_Child);
               Update_Node_Bounds (R_Child);

               Subdivide (L_Child);
               Subdivide (R_Child);
            end if;
         end if;
      end Subdivide;

      Start_Time     : Time := Clock;
      Build_Duration : Duration;
   begin
      Ada.Text_IO.Put_Line ("Building BVH...");
      for I in C'Range loop
         case W (I).Kind is
            when Triangle =>
               C (I) := (3.0 * W (I).T.Origin + W (I).T.V1 + W (I).T.V2) / 3.0;

            when Triangle_N =>
               C (I) :=
                 (3.0 * W (I).TN.Origin + W (I).TN.V1 + W (I).TN.V2) / 3.0;

            when Sphere =>
               C (I) := W (I).S.Origin;
         end case;
      end loop;

      H.Append (B);
      H (0).Left := 0;
      H (0).Right := 0;
      H (0).First := W'First;
      H (0).Last := W'Last;
      Update_Node_Bounds (0);
      Subdivide (0);

      Point_Array_Free (C);
      Build_Duration := Clock - Start_Time;
      Ada.Text_IO.Put_Line ("Built BVH in" & Build_Duration'Img & " seconds.");
      return Vec_To_Array (H);
   end Build_BVH;

   function BVH_Depth (H : BVH) return Natural is
      function Helper (Node : BVH_Node) return Natural is
      begin
         if H (Node).Left = 0 then
            return 1;
         else
            return
              1
              + Natural'Max (Helper (H (Node).Left), Helper (H (Node).Right));
         end if;
      end Helper;
   begin
      return Helper (0);
   end BVH_Depth;

   function Trace
     (Self        : World;
      Hierarchy   : BVH;
      Mats        : Material_Map;
      G           : in out Random.Rng;
      R_Orig      : Ray;
      Max_Bounces : Positive;
      Background  : Rgb) return Rgb
   is
      use Primitives;
      use Materials;

      R   : Ray := R_Orig;
      C   : Rgb := WHITE;
      B   : Natural := 0;
      H   : World_Hit;
      --  Tmp : Hit;
      Dir : Vec3;
   begin
      loop
         exit when B = Max_Bounces;
         H := Intersect (Self, Hierarchy, R, 1.0e-10, Real'Last);
         if H.H.Hit = Primitives.None then
            C := C * Background;
            exit;
         end if;
         exit when
           Materials.Emit (Mats (H.M), H.H, G, R.Direction, C)
           = Materials.Absorb;

         R.Origin := H.H.P;
         B := B + 1;
      end loop;
      return C;
   end Trace;

   function Test_World (M : out Material_Map) return World is
      S1 : constant Primitives.Sphere := ((5.0, 0.0, 3.0), 1.0);
      S2 : constant Primitives.Sphere := ((5.0, 0.5, -1.0), 1.5);
      S3 : constant Primitives.Sphere := ((5.0, -1_000.0, 0.0), 999.0);
      S4 : constant Primitives.Sphere := ((4.0, -0.5, 1.5), 0.5);
      S5 : constant Primitives.Sphere := ((9.0, 2.0, 1.5), 3.0);
      T1 : constant Primitives.Triangle :=
        ((4.0, -1.0, 0.0), Z, 2.0 * Y, -2.0 * X);

      Ground  : constant Materials.Material :=
        (Materials.Emissive, WHITE, 1.0);
      Gold    : constant Materials.Material :=
        (Materials.Specular, (0.75, 0.5, 0.125), 0.05);
      Glass   : constant Materials.Material :=
        (Materials.Dielectric, WHITE, 1.25, 0.0);
      Teal    : constant Materials.Material :=
        (Materials.Plastic, (0.125, 0.5, 0.75), 2.0, 0.0);
      Magenta : constant Materials.Material :=
        (Materials.Diffuse, (0.75, 0.125, 0.5));

      W : World := new World_Array (0 .. 5);
   begin
      M (0) := Gold;
      W (0) := (Sphere, 0, S1);

      M (1) := Glass;
      W (1) := (Sphere, 1, S2);

      M (2) := Ground;
      W (2) := (Sphere, 2, S3);

      M (3) := Magenta;
      W (3) := (Sphere, 3, S4);
      W (4) := (Triangle, 3, T1);

      M (4) := Teal;
      W (5) := (Sphere, 4, S5);
      return W;
   end Test_World;

end Scene;
