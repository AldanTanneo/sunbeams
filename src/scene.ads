with Colour;   use Colour;
with Geometry; use Geometry;
with Materials;
with Primitives;
with Random;
with Utils;    use Utils;

package Scene is

   type Material_Index is new Integer range 0 .. 2 ** 8 - 1;
   type Material_Map is array (Material_Index range <>) of Materials.Material;

   type Object_Kind is (Sphere, Triangle, Triangle_N);
   type Object (Kind : Object_Kind := Sphere) is record
      Material : Material_Index;
      case Kind is
         when Sphere =>
            S : Primitives.Sphere;

         when Triangle =>
            T : Primitives.Triangle;

         when Triangle_N =>
            TN : Primitives.Triangle_N;
      end case;
   end record;

   function Intersect
     (Self : Object; R : Ray; Min, Max : Real) return Primitives.Hit;

   type World_Array is array (Natural range <>) of Object;
   type World is access World_Array;

   type Aabb is record
      Min : Point := (Real'Last, Real'Last, Real'Last);
      Max : Point := (Real'First, Real'First, Real'First);
   end record;
   type BVH_Node is new Natural;
   type Box is record
      Bounds      : Aabb;
      First, Last : Natural;
      Left, Right : BVH_Node;
   end record;
   type BVH_Array is array (BVH_Node range <>) of Box;
   type BVH is access BVH_Array;

   function Build_BVH (W : World) return BVH;
   function BVH_Depth (H : BVH) return Natural;

   type World_Hit is record
      H : Primitives.Hit;
      M : Material_Index;
   end record;

   function Intersect (Box : Aabb; R : Ray; Min, Max : Real) return Real;

   function Intersect
     (Self      : World;
      Hierarchy : BVH;
      R         : Ray;
      Min, Max  : Real;
      Node      : BVH_Node := 0) return World_Hit;

   function Trace
     (Self        : World;
      Hierarchy   : BVH;
      Mats        : Material_Map;
      G           : in out Random.Rng;
      R_Orig      : Ray;
      Max_Bounces : Positive;
      Background  : Rgb) return Rgb;

   function Test_World (M : out Material_Map) return World;

end Scene;
