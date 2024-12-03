with Utils; use Utils;
with Geometry;

package Colour is
   type Rgb is record
      R, G, B : Real := 0.0;
   end record;

   BLACK   : constant Rgb := (0.0, 0.0, 0.0);
   RED     : constant Rgb := (1.0, 0.0, 0.0);
   GREEN   : constant Rgb := (0.0, 1.0, 0.0);
   BLUE    : constant Rgb := (0.0, 0.0, 1.0);
   YELLOW  : constant Rgb := (1.0, 1.0, 0.0);
   CYAN    : constant Rgb := (0.0, 1.0, 1.0);
   MAGENTA : constant Rgb := (1.0, 0.0, 1.0);
   WHITE   : constant Rgb := (1.0, 1.0, 1.0);

   function "+" (Self, Other : Rgb) return Rgb;
   function "*" (Self : Real; Other : Rgb) return Rgb;
   function "*" (Self : Rgb; Other : Real) return Rgb;
   function "*" (Self, Other : Rgb) return Rgb;
   function "/" (Self : Rgb; Other : Real) return Rgb;

   function Clamp (Self : Rgb; Low, High : Real) return Rgb;
   function Sqrt (Self : Rgb) return Rgb;
   function From_Vec3 (V : Geometry.Vec3) return Rgb;
end Colour;
