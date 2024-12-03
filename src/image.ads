with Colour; use Colour;

package Image is
   type Image_Array is array (Natural range <>, Natural range <>) of Rgb;
   type Image is access Image_Array;
   subtype Chunk is Image;

   function Create (Width, Height : Positive) return Image;
   procedure Gamma_Correct (Img : in out Image);
   procedure Write_PNG (Img : Image; Filename : String);
end Image;
