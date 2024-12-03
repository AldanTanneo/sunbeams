package body Materials is

   function Reflectance (Cos_Theta, Refraction_Ratio : Real) return Real is
      R0 : Real := (1.0 - Refraction_Ratio) / (1.0 + Refraction_Ratio);
   begin
      R0 := R0 ** 2;
      return R0 + (1.0 - R0) * (1.0 - Cos_Theta) ** 5;
   end Reflectance;

   function Emit
     (M      : Material;
      H      : Hit;
      G      : in out Random.Rng;
      Dir    : in out Vec3;
      Colour : in out Rgb) return Surface
   is
      R2, R_Cos_Theta, Cos_Theta, Sin_Theta, Refraction_Ratio : Real;
      Orth_Out, Parr_Out                                      : Vec3;
   begin
      Colour := Colour * M.Colour;

      case M.Kind is
         when Diffuse =>
            Dir := H.Normal + 0.99 * Random_Unit (G);
            return Transmit;

         when Specular =>
            Dir :=
              Dir
              - Dot (Dir, H.Normal)
                * (M.Roughness * Random_Unit (G) + 2.0 * H.Normal);
            return Transmit;

         when Dielectric | Plastic =>
            R2 := Length_Squared (Dir);
            R_Cos_Theta := -Dot (Dir, H.Normal);
            Cos_Theta := Real'Min (R_Cos_Theta / Sqrt (R2), 1.0);
            Sin_Theta := Sqrt (1.0 - Cos_Theta ** 2);
            Refraction_Ratio :=
              (if H.Hit = Front then 1.0 / M.Index else M.Index);

            if Refraction_Ratio * Sin_Theta > 1.0
              or else Reflectance (Cos_Theta, Refraction_Ratio)
                      > Random.Gen (G)
            then
               Dir :=
                 Dir
                 + R_Cos_Theta
                   * (M.Fuzziness * Random_Unit (G) + 2.0 * H.Normal);
            elsif M.Kind = Dielectric then
               Orth_Out := Refraction_Ratio * (Dir + R_Cos_Theta * H.Normal);
               Parr_Out := -Sqrt (R2 - Length_Squared (Orth_Out)) * H.Normal;
               Dir := Orth_Out + Parr_Out;
            else
               Dir := H.Normal + 0.99 * Random_Unit (G);
            end if;
            return Transmit;

         when Emissive =>
            Colour := Colour * M.Intensity;
            return Absorb;
      end case;
   end Emit;
end Materials;
