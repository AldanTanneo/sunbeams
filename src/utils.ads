with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Interfaces; use Interfaces;

package Utils is
   type Real is new Long_Float;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);

   function Sqrt (Value : Real) return Real renames Math.Sqrt;
   function Tan (Value : Real) return Real renames Math.Tan;

   package Real_IO is new Ada.Text_IO.Float_IO (Real);

   subtype U8 is Unsigned_8;
   subtype U16 is Unsigned_16;
   subtype U32 is Unsigned_32;
   subtype U64 is Unsigned_64;

   function Shl (Value : U64; Shift : Natural) return U64 renames Shift_Left;
   function Shr (Value : U64; Shift : Natural) return U64 renames Shift_Right;
   function Rotl (Value : U64; Rot : Natural) return U64 renames Rotate_Left;
   function Rotr (Value : U64; Rot : Natural) return U64 renames Rotate_Right;

   function Clamp (Value, Low, High : Real) return Real;

   function Degrees_To_Radians (Deg : Real) return Real;
end Utils;
