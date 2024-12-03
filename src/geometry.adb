with Random;

package body Geometry is
   pragma Suppress_All;

   function Get (V : Vec3; A : Axis) return Real is
   begin
      case A is
         when AX =>
            return V.X;

         when AY =>
            return V.Y;

         when AZ =>
            return V.Z;
      end case;
   end Get;

   function "-" (Self : Vec3) return Vec3 is
   begin
      return (-Self.X, -Self.Y, -Self.Z);
   end "-";

   function "+" (Self, Other : Vec3) return Vec3 is
   begin
      return (Self.X + Other.X, Self.Y + Other.Y, Self.Z + Other.Z);
   end "+";

   function "-" (Self, Other : Vec3) return Vec3 is
   begin
      return (Self.X - Other.X, Self.Y - Other.Y, Self.Z - Other.Z);
   end "-";

   function "*" (Self : Real; Other : Vec3) return Vec3 is
   begin
      return (Other.X * Self, Other.Y * Self, Other.Z * Self);
   end "*";

   function "*" (Self : Vec3; Other : Real) return Vec3 is
   begin
      return (Self.X * Other, Self.Y * Other, Self.Z * Other);
   end "*";

   function "/" (Self : Vec3; Other : Real) return Vec3 is
   begin
      return Self * (1.0 / Other);
   end "/";

   function "/" (Self, Other : Vec3) return Vec3 is
   begin
      return (Self.X / Other.X, Self.Y / Other.Y, Self.Z / Other.Z);
   end "/";

   function Cross (Self, Other : Vec3) return Vec3 is
   begin
      return
        (Self.Y * Other.Z - Self.Z * Other.Y,
         Self.Z * Other.X - Self.X * Other.Z,
         Self.X * Other.Y - Self.Y * Other.X);
   end Cross;

   function Dot (Self, Other : Vec3) return Real is
   begin
      return Self.X * Other.X + Self.Y * Other.Y + Self.Z * Other.Z;
   end Dot;

   function Length_Squared (Self : Vec3) return Real is
   begin
      return Self.X**2 + Self.Y**2 + Self.Z**2;
   end Length_Squared;

   function Length (Self : Vec3) return Real is
   begin
      return Sqrt (Length_Squared (Self));
   end Length;

   function Unit (Self : Vec3) return Vec3 is
   begin
      return Self / (Length (Self));
   end Unit;

   function Reflect (V, N : Vec3) return Vec3 is
   begin
      return V - 2.0 * Dot (V, N) * N;
   end Reflect;

   function Ray_At (R : Ray; T : Real) return Point is
   begin
      return R.Origin + T * R.Direction;
   end Ray_At;

   function Min (Self, Other : Vec3) return Vec3 is
   begin
      return
        (Real'Min (Self.X, Other.X), Real'Min (Self.Y, Other.Y),
         Real'Min (Self.Z, Other.Z));
   end Min;

   function Max (Self, Other : Vec3) return Vec3 is
   begin
      return
        (Real'Max (Self.X, Other.X), Real'Max (Self.Y, Other.Y),
         Real'Max (Self.Z, Other.Z));
   end Max;

   function Rotate (Self : Vec3; A : Axis; Angle : Real) return Vec3 is
      Cos : Real := Math.Cos (Angle);
      Sin : Real := Math.Sin (Angle);
   begin
      case A is
         when AX =>
            return
              (Self.X, Cos * Self.Y - Sin * Self.Z,
               Sin * Self.Y + Cos * Self.Z);
         when AY =>
            return
              (Sin * Self.Z + Cos * Self.X, Self.Y,
               Cos * Self.Z - Sin * Self.X);
         when AZ =>
            return
              (Cos * Self.X - Sin * Self.Y, Sin * Self.X + Cos * Self.Y,
               Self.Z);
      end case;
   end Rotate;

   function Rotate (Self : Vec3; Angles : Vec3) return Vec3 is
      Res : Vec3 := Self;
   begin
      Res := Rotate (Res, AX, Angles.X);
      Res := Rotate (Res, AY, Angles.Y);
      Res := Rotate (Res, AZ, Angles.Z);
      return Res;
   end Rotate;

   function Random_Vec3 (G : in out Random.Rng) return Vec3 is
   begin
      return
        (Random.Gen (G, -1.0, 1.0), Random.Gen (G, -1.0, 1.0),
         Random.Gen (G, -1.0, 1.0));
   end Random_Vec3;

   function Random_Unit (G : in out Random.Rng) return Vec3 is
      V : Vec3;
      L : Real;
   begin
      loop
         V := Random_Vec3 (G);
         L := Length_Squared (V);
         exit when L > 1.0e-15 and then L <= 1.0;
      end loop;
      return V / Sqrt (L);
   end Random_Unit;

end Geometry;
