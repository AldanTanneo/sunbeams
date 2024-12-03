with Colour;     use Colour;
with Geometry;   use Geometry;
with Primitives; use Primitives;
with Utils;      use Utils;
with Random;

package Materials is
   type Material_Kind is (Diffuse, Specular, Dielectric, Plastic, Emissive);

   type Material (Kind : Material_Kind := Diffuse) is record
      Colour : Rgb;
      case Kind is
         when Diffuse =>
            null;

         when Specular =>
            Roughness : Real;

         when Dielectric
               | Plastic =>
            Index     : Real;
            Fuzziness : Real;

         when Emissive =>
            Intensity : Real;
      end case;
   end record;

   type Surface is (Absorb, Transmit);

   function Emit
     (M      : Material;
      H      : Hit;
      G      : in out Random.Rng;
      Dir    : in out Vec3;
      Colour : in out Rgb) return Surface;

end Materials;
