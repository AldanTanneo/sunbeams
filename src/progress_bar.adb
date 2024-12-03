with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Environment_Variables;
with Ada.Characters.Latin_1;

package body Progress_Bar is
   function Default_Config (Width : Natural) return Config is
      Cfg : Config := new Bar_Config;
   begin
      Cfg.Width := Width;
      Cfg.Bar_Empty := '.';
      Cfg.Bar_Full := '#';
      Cfg.Bar_Start := '[';
      Cfg.Bar_End := ']';
      return Cfg;
   end;

   task body Progress is
      Work, State, Term_Width : Natural;
      Width_Str               : String :=
        Ada.Environment_Variables.Value ("COLUMNS", "80");
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      Ada.Integer_Text_IO.Get (Width_Str, Term_Width, Work);
      Term_Width := Term_Width - 2;
      Work := 0;
      State := 0;
      for I in 0 .. Term_Width loop
         Put ([Cfg.Bar_Empty]);
      end loop;
      Put ([Cfg.Bar_End]);
      Flush;
      Put ([Ada.Characters.Latin_1.CR, Cfg.Bar_Start]);
      loop
         select
            accept Next do
               Work := Work + 1;
            end Next;
            if Work * Term_Width > State * Cfg.Width then
               Put ([Cfg.Bar_Full]);
               State := State + 1;
               Flush;
            end if;
         or
            accept Finish do
               Put_Line ("");
            end Finish;
            exit;
         or
            terminate;
         end select;
      end loop;
   end Progress;
end Progress_Bar;
