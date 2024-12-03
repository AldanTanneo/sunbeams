package body Camera is

   function Height_To_Width
     (AR : Aspect_Ratio; Height : Positive) return Positive is
   begin
      return (Height * AR.Width) / AR.Height;
   end Height_To_Width;

   function Width_To_Height
     (AR : Aspect_Ratio; Width : Positive) return Positive is
   begin
      return (Width * AR.Height) / AR.Width;
   end Width_To_Height;

   function Create
     (Origin, Look_At : Point;
      AR              : Aspect_Ratio;
      Fov             : Field_Of_View;
      Up              : Vec3 := Y) return Camera
   is
      Direction      : constant Vec3 := Look_At - Origin;
      Focal_Distance : constant Real := Length (Direction);

      Viewport_Height : constant Real :=
        2.0 * Focal_Distance * Tan (Degrees_To_Radians (Fov) / 2.0);
      Viewport_Width  : constant Real :=
        Viewport_Height * Real (AR.Width) / Real (AR.Height);

      Unit_Dir : constant Vec3 := Direction / Focal_Distance;

      U : Vec3 := Cross (Unit_Dir, Unit (Up));
      V : Vec3 := Cross (U, Unit_Dir);
   begin
      U := U * Viewport_Width;
      V := -V * Viewport_Height;
      return (Origin, Direction - U / 2.0 - V / 2.0, U, V);
   end Create;

   function Cast (Cam : Camera; U, V : Screen_Coord) return Ray is
   begin
      return (Cam.Origin, Cam.To_Viewport + U * Cam.U + V * Cam.V);
   end Cast;

end Camera;
