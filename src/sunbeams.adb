with Camera;
with Colour;   use Colour;
with Image;
with Geometry; use Geometry;
with Random;
with Utils;    use Utils;
with Scene;
with Parser;
with Progress_Bar;
with Threadpool;

with Ada.Environment_Variables;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;
with System.Multiprocessors; use System.Multiprocessors;

procedure Sunbeams is
   Background           : Rgb;
   Samples, Max_Bounces : Positive;
   Cam                  : Camera.Camera;
   Img                  : Image.Image;
   M                    : Scene.Material_Map (0 .. 255);
   W                    : Scene.World :=
     Parser.Parse_Scene_File
       ("scene.json", M, Cam, Img, Samples, Max_Bounces, Background);

   U_Scale : constant Real := 1.0 / Real (Img'Length(1));
   V_Scale : constant Real := 1.0 / Real (Img'Length(2));
   H       : constant Scene.BVH := Scene.Build_BVH (W);

   function Num_Threads return Positive is
      P, L : Positive;
   begin
      if Ada.Environment_Variables.Exists ("SUNBEAMS_THREADS") then
         Ada.Integer_Text_IO.Get
           (Ada.Environment_Variables.Value ("SUNBEAMS_THREADS"), P, L);
      else
         P := Positive (Number_Of_CPUs);
      end if;
      return P;
   end Num_Threads;
   Parallelism : constant Positive := Num_Threads;

   procedure Process_Column (G : in out Random.Rng; X : Natural) is
      U, V        : Real;
      UV_Ray      : Ray;
      Acc, Sample : Rgb;
   begin
      for Y in Img'Range(2) loop
         Acc := BLACK;
         for S in 1 .. Samples loop
            U := Real (X) + Random.Gen (G);
            V := Real (Y) + Random.Gen (G);
            UV_Ray := Camera.Cast (Cam, U * U_Scale, V * V_Scale);
            Sample :=
              Scene.Trace (W, H, M, G, UV_Ray, Max_Bounces, Background);
            Acc := Acc + Sample;
         end loop;
         Img (X, Y) := Acc / Real (Samples);
      end loop;
   end Process_Column;

   package Work_Queue is new
     Threadpool (Natural, Random.Rng, Process_Column, Random.Seed_Time);

   Pool    : Work_Queue.Thread_Pool (Parallelism);
   Bar_Cfg : Progress_Bar.Config :=
     Progress_Bar.Default_Config (Img'Length(1));
   Bar     : Progress_Bar.Progress (Bar_Cfg);
begin
   Put_Line ("BVH Nodes =" & H'Length'Img);
   Put_Line ("BVH Depth =" & Scene.BVH_Depth (H)'Img);
   Put_Line ("Running on" & Parallelism'Img & " processors");

   Bar.Start;
   for X in Img'Range(1) loop
      Pool.Dispatch (X);
      Bar.Next;
   end loop;
   Pool.Finish;
   Bar.Finish;

   Put_Line ("Writing image...");
   Image.Gamma_Correct (Img);
   Image.Write_PNG (Img, "test.png");
end Sunbeams;
