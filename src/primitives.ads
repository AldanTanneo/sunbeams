with Geometry; use Geometry;
with Utils;    use Utils;
with Colour;   use Colour;
with Random;

package Primitives is
   type Hit_Kind is (None, Front, Back);
   type Hit is record
      Hit    : Hit_Kind;
      Time   : Real;
      P      : Point;
      Normal : Vec3;
   end record;

   type Sphere is record
      Origin : Point;
      Radius : Real;
   end record;

   type Triangle is record
      Origin    : Point;
      V1, V2, N : Vec3;
   end record;

   type Triangle_N is record
      Origin     : Point;
      V1, V2, N  : Vec3;
      N0, N1, N2 : Vec3;
   end record;

   function Create (O, A, B : Point) return Triangle;
   function Create (O, A, B : Point; NO, NA, NB : Vec3) return Triangle_N;

   function Intersect (Self : Sphere; R : Ray; Min, Max : Real) return Hit;
   function Intersect (Self : Triangle; R : Ray; Min, Max : Real) return Hit;
   function Intersect (Self : Triangle_N; R : Ray; Min, Max : Real) return Hit;
end Primitives;
