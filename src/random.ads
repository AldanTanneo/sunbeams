with Utils; use Utils;

package Random is
   --  Fast global random number generation based on Xoroshiro128+

   type Rng is private;

   procedure Seed (G : out Rng; S0, S1 : U64);
   --  Initialize the RNG with a random seed
   procedure Seed (G : out Rng; Value : U64);
   --  Initialize the RNG with a random seed
   procedure Seed_Time (G : out Rng);
   --  Initialize the RNG with a time-based seed

   function Gen (G : in out Rng) return U64;
   function Gen (G : in out Rng) return Real;
   function Gen (G : in out Rng; Low, High : Real) return Real;

private
   type Rng is record
      S0, S1 : U64;
   end record;
end Random;
