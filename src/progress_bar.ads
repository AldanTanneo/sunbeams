package Progress_Bar is
   type Bar_Config is record
      Width               : Natural;
      Bar_Empty, Bar_Full : Character;
      Bar_Start, Bar_End  : Character;
   end record;
   type Config is access Bar_Config;

   function Default_Config (Width : Natural) return Config;

   task type Progress (Cfg : Config) is
      entry Start;
      entry Next;
      entry Finish;
   end Progress;
end Progress_Bar;
