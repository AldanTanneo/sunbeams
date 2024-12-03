generic
   type T is private;
   type Context is private;
   with procedure Work_Unit (Ctx : in out Context; Param : T);
   with procedure Init_Context (Ctx : out Context);
package Threadpool is
   type Thread_Id is limited private;

   task type Thread_Pool (Count : Natural) is
      entry Dispatch (Param : T);
      entry Finish;
      entry Ready (Id : Thread_Id);
   end Thread_Pool;

private
   type Thread_Id is new Positive;

end Threadpool;
