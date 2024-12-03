with Colour;
with Camera;
with Image;
with Scene;

package Parser is
   function Parse_Scene_File
     (Filename             : String;
      Mat                  : out Scene.Material_Map;
      Cam                  : out Camera.Camera;
      Img                  : out Image.Image;
      Samples, Max_Bounces : out Positive;
      Background           : out Colour.Rgb) return Scene.World;
end Parser;
