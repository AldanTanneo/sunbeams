with Image_IO; use Image_IO;
with Image_IO.Operations;

with Utils; use Utils;

package body Image is
   function Create (Width, Height : Positive) return Image is
      Img : Image := new Image_Array (0 .. Width - 1, 0 .. Height - 1);
   begin
      return Img;
   end Create;

   procedure Gamma_Correct (Img : in out Image) is
   begin
      for X in Img'Range(1) loop
         for Y in Img'Range(2) loop
            Img (X, Y) := Sqrt (Clamp (Img (X, Y), 0.0, 1.0));
         end loop;
      end loop;
   end Gamma_Correct;

   function Real_To_U8 (Value : Real) return U8 is
   begin
      return U8 (Real'Floor (Value * Real'Pred (256.0)));
   end Real_To_U8;

   procedure Write_PNG (Img : Image; Filename : String) is
      Raw_Data : access Image_Data :=
        new Image_Data (Img'Range(2), Img'Range(1));
   begin
      for X in Img'Range(1) loop
         for Y in Img'Range(2) loop
            Raw_Data (Y, X) :=
              (Red   => Real_To_U8 (Img (X, Y).R),
               Green => Real_To_U8 (Img (X, Y).G),
               Blue  => Real_To_U8 (Img (X, Y).B));
         end loop;
      end loop;
      Operations.Write_PNG (Filename, Raw_Data.all);
   end Write_PNG;
end Image;
