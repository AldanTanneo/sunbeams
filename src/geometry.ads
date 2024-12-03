with Utils; use Utils;
with Random;

package Geometry is
   type Vec3 is record
      X, Y, Z : Real := 0.0;
   end record;
   subtype Point is Vec3;

   ZERO : constant Vec3 := (0.0, 0.0, 0.0);
   ONES : constant Vec3 := (1.0, 1.0, 1.0);
   X    : constant Vec3 := (1.0, 0.0, 0.0);
   Y    : constant Vec3 := (0.0, 1.0, 0.0);
   Z    : constant Vec3 := (0.0, 0.0, 1.0);

   type Axis is (AX, AY, AZ);
   function Get (V : Vec3; A : Axis) return Real;

   function "-" (Self : Vec3) return Vec3;
   function "+" (Self, Other : Vec3) return Vec3;
   function "-" (Self, Other : Vec3) return Vec3;
   function "*" (Self : Real; Other : Vec3) return Vec3;
   function "*" (Self : Vec3; Other : Real) return Vec3;
   function "/" (Self : Vec3; Other : Real) return Vec3;
   function "/" (Self, Other : Vec3) return Vec3;

   function Cross (Self, Other : Vec3) return Vec3;
   function Dot (Self, Other : Vec3) return Real;
   function Length_Squared (Self : Vec3) return Real;
   function Length (Self : Vec3) return Real;
   function Unit (Self : Vec3) return Vec3;
   function Reflect (V, N : Vec3) return Vec3;
   function Min (Self, Other : Vec3) return Vec3;
   function Max (Self, Other : Vec3) return Vec3;
   function Rotate (Self: Vec3; A: Axis; Angle: Real) return Vec3;
   function Rotate (Self: Vec3; Angles: Vec3) return Vec3;

   type Ray is record
      Origin    : Point;
      Direction : Vec3;
   end record;

   function Ray_At (R : Ray; T : Real) return Point;

   function Random_Unit (G : in out Random.Rng) return Vec3;
end Geometry;
