with Geometry; use Geometry;

package body Colour is

   function "+" (Self, Other : Rgb) return Rgb is
   begin
      return (Self.R + Other.R, Self.G + Other.G, Self.B + Other.B);
   end "+";

   function "*" (Self : Real; Other : Rgb) return Rgb is
   begin
      return (Other.R * Self, Other.G * Self, Other.B * Self);
   end "*";

   function "*" (Self : Rgb; Other : Real) return Rgb is
   begin
      return (Self.R * Other, Self.G * Other, Self.B * Other);
   end "*";

   function "*" (Self, Other : Rgb) return Rgb is
   begin
      return (Self.R * Other.R, Self.G * Other.G, Self.B * Other.B);
   end "*";

   function "/" (Self : Rgb; Other : Real) return Rgb is
   begin
      return Self * (1.0 / Other);
   end "/";

   function Clamp (Self : Rgb; Low, High : Real) return Rgb is
   begin
      return
        (Clamp (Self.R, Low, High),
         Clamp (Self.G, Low, High),
         Clamp (Self.B, Low, High));
   end Clamp;

   function Sqrt (Self : Rgb) return Rgb is
   begin
      return (Sqrt (Self.R), Sqrt (Self.G), Sqrt (Self.B));
   end Sqrt;

   function From_Vec3 (V : Vec3) return Rgb is
      W : constant Vec3 := (Unit (V) + ONES) / 2.0;
   begin
      return (W.X, W.Y, W.Z);
   end From_Vec3;

end Colour;
