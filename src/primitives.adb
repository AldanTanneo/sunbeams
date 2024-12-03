package body Primitives is

   function Create (O, A, B : Point) return Triangle is
   begin
      return (O, A - O, B - O, Cross (A - O, B - O));
   end Create;

   function Create (O, A, B : Point; NO, NA, NB : Vec3) return Triangle_N is
   begin
      return (O, A - O, B - O, Cross (A - O, B - O), NO, NA, NB);
   end Create;

   function Intersect (Self : Sphere; R : Ray; Min, Max : Real) return Hit is
      Rec : Hit;
      Oc  : constant Vec3 := Self.Origin - R.Origin;
      A   : constant Real := Length_Squared (R.Direction);
      H   : constant Real := Dot (R.Direction, Oc);
      C   : constant Real := Length_Squared (Oc) - Self.Radius ** 2;
      D   : constant Real := H ** 2 - A * C;
   begin
      Rec.Hit := None;
      if D >= 0.0 then
         Rec.Time := (H - Sqrt (D)) / A;
         if Rec.Time <= Min then
            Rec.Time := (H + Sqrt (D)) / A;
         end if;
         if Rec.Time <= Min or else Rec.Time >= Max then
            return Rec;
         else
            Rec.Hit := Front;
            Rec.P := Ray_At (R, Rec.Time);
            Rec.Normal := (Rec.P - Self.Origin) / Self.Radius;

            if Dot (R.Direction, Rec.Normal) > 0.0 then
               Rec.Normal := -Rec.Normal;
               Rec.Hit := Back;
            end if;
         end if;
      end if;
      return Rec;
   end Intersect;

   function Intersect (Self : Triangle; R : Ray; Min, Max : Real) return Hit is
      Det   : constant Real := Dot (R.Direction, Self.N);
      Scale : constant Real := 1.0 / Det;

      Rec : Hit;

      Lambda, Mu               : Real;
      O_to_V, Dir_Cross_O_to_V : Vec3;
   begin
      Rec.Hit := None;
      if Det > -1.0e-8 and then Det < 1.0e-8 then
         return Rec;
      end if;

      O_to_V := Self.Origin - R.Origin;
      Rec.Time := Scale * Dot (O_to_V, Self.N);
      if Rec.Time < Min or else Rec.Time > Max then
         return Rec;
      end if;

      Dir_Cross_O_to_V := Cross (R.Direction, O_to_V);

      Lambda := Scale * Dot (Dir_Cross_O_to_V, Self.V1);
      if Lambda < 0.0 or else Lambda > 1.0 then
         return Rec;
      end if;

      Mu := -Scale * Dot (Dir_Cross_O_to_V, Self.V2);
      if Mu < 0.0 or else Lambda + Mu > 1.0 then
         return Rec;
      end if;

      Rec.Hit := Front;
      Rec.P := Ray_At (R, Rec.Time);
      Rec.Normal := Unit (Self.N);
      if Dot (R.Direction, Rec.Normal) > 0.0 then
         Rec.Hit := Back;
         Rec.Normal := -Rec.Normal;
      end if;
      return Rec;
   end Intersect;

   function Intersect (Self : Triangle_N; R : Ray; Min, Max : Real) return Hit
   is
      Det   : constant Real := Dot (R.Direction, Self.N);
      Scale : constant Real := 1.0 / Det;

      Rec : Hit;

      Lambda, Mu               : Real;
      O_to_V, Dir_Cross_O_to_V : Vec3;
   begin
      Rec.Hit := None;
      if Det > -1.0e-8 and then Det < 1.0e-8 then
         return Rec;
      end if;

      O_to_V := Self.Origin - R.Origin;
      Rec.Time := Scale * Dot (O_to_V, Self.N);
      if Rec.Time < Min or else Rec.Time > Max then
         return Rec;
      end if;

      Dir_Cross_O_to_V := Cross (R.Direction, O_to_V);

      Lambda := Scale * Dot (Dir_Cross_O_to_V, Self.V1);
      if Lambda < 0.0 or else Lambda > 1.0 then
         return Rec;
      end if;

      Mu := -Scale * Dot (Dir_Cross_O_to_V, Self.V2);
      if Mu < 0.0 or else Lambda + Mu > 1.0 then
         return Rec;
      end if;

      Rec.Hit := Front;
      Rec.P := Ray_At (R, Rec.Time);
      Rec.Normal :=
        Unit (Lambda * Self.N2 + Mu * Self.N1 + (1.0 - Lambda - Mu) * Self.N0);
      if Dot (R.Direction, Rec.Normal) > 0.0 then
         Rec.Hit := Back;
         Rec.Normal := -Rec.Normal;
      end if;
      return Rec;
   end Intersect;

end Primitives;
