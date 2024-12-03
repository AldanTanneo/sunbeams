package body Threadpool is

   task body Thread_Pool is

      task type Chunk is
         entry Start (Id : Thread_Id);
         entry Process (Param : T);
         entry Finish;
      end Chunk;

      task body Chunk is
         Thr_Id        : Thread_Id;
         Current_Param : T;
         Ctx           : Context;
      begin

         accept Start (Id : Thread_Id) do
            Thr_Id := Id;
         end Start;
         Init_Context (Ctx);

         loop
            Ready (Thr_Id);
            select
               accept Process (Param : T) do
                  Current_Param := Param;
               end Process;
               Work_Unit (Ctx, Current_Param);
            or
               accept Finish;
               exit;
            end select;
         end loop;

      end Chunk;

      Pool           : array (1 .. Thread_Id (Count)) of Chunk;
      Working        : Natural;
      Current_Param  : T;
      Current_Thread : Thread_Id;
   begin

      for Id in Pool'Range loop
         Pool (Id).Start (Id);
         Working := Working + 1;
      end loop;

      loop
         select
            accept Dispatch (Param : T) do
               Current_Param := Param;
            end Dispatch;
            accept Ready (Id : Thread_Id) do
               Current_Thread := Id;
            end Ready;
            Pool (Current_Thread).Process (Current_Param);
         or
            accept Finish do
               while Working > 0 loop
                  accept Ready (Id : Thread_Id) do
                     Current_Thread := Id;
                  end Ready;
                  Pool (Current_Thread).Finish;
                  Working := Working - 1;
               end loop;
            end Finish;
            exit;
         end select;
      end loop;

   end Thread_Pool;

end Threadpool;
