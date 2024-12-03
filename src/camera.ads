with Geometry; use Geometry;
with Utils;    use Utils;

package Camera is
   type Aspect_Ratio is record
      Width  : Positive;
      Height : Positive;
   end record;

   function Height_To_Width
     (AR : Aspect_Ratio; Height : Positive) return Positive;
   function Width_To_Height
     (AR : Aspect_Ratio; Width : Positive) return Positive;

   subtype Field_Of_View is Real range 0.0 .. 180.0;
   subtype Screen_Coord is Real range 0.0 .. 1.0;

   type Camera is record
      Origin            : Point;
      --  VFov, HFov        : Field_Of_View;
      To_Viewport, U, V : Vec3;
   end record;

   function Create
     (Origin, Look_At : Point;
      AR              : Aspect_Ratio;
      Fov             : Field_Of_View;
      Up              : Vec3 := Y) return Camera;
   function Cast (Cam : Camera; U, V : Screen_Coord) return Ray;
end Camera;
