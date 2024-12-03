with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces; use Interfaces;

package body Random is
   type S is array (0 .. 1) of U64;

   XorConst : constant S := [16#1234567890ABCDEF#, 16#FEDCBA0987654321#];
   State    : S := XorConst;

   procedure Seed (G : out Rng; S0, S1 : U64) is
   begin
      G := (S0 xor XorConst (0), S1 xor XorConst (1));
   end Seed;

   procedure Seed (G : out Rng; Value : U64) is
      S0, S1 : U64;
   begin
      Seed (G, Value, Value);
      --  Mix the initial seed a little more
      for I in 1 .. 10 loop
         S1 := Gen (G);
         S0 := Gen (G);
         Seed (G, S0, S1);
      end loop;
   end Seed;

   procedure Seed_Time (G : out Rng) is
      use Ada.Calendar;
   begin
      Seed (G, U64 (Conversions.To_Unix_Nano_Time (Clock)));
   end Seed_Time;

   function Gen (G : in out Rng) return U64 is
      S0  : constant U64 := G.S0;
      S1  : U64 := G.S1;
      Res : constant U64 := Rotl (S0 * 5, 7) * 9;
   begin
      S1 := S1 xor S0;
      G.S0 := Rotl (S0, 24) xor S1 xor Shl (S1, 16);
      G.S1 := Rotl (S1, 37);
      return Res;
   end Gen;

   function Gen (G : in out Rng) return Real is
      Z : constant U64 := Gen (G);
   begin
      --  Only take the 53 most significant bits
      return Real (Z) / (2.0 ** 64);
   end Gen;

   function Gen (G : in out Rng; Low, High : Real) return Real is
      Z : constant Real := Gen (G);
   begin
      return (High - Low) * Z + Low;
   end Gen;

end Random;
