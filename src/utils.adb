with Ada.Numerics; use Ada.Numerics;

package body Utils is

   function Clamp (Value, Low, High : Real) return Real is
   begin
      return Real'Min (Real'Max (Value, Low), High);
   end Clamp;

   function Degrees_To_Radians (Deg : Real) return Real is
   begin
      return Deg * Pi / 180.0;
   end Degrees_To_Radians;

end Utils;
