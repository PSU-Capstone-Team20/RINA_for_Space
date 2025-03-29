with Ada.Text_IO; use Ada.Text_IO;

package body Log is

   procedure Log_Message(Level : String; Msg : String) is
   begin
      Put_Line("[" & Level & "] : " &  Msg);
   end Log_Message;

   procedure Debug(Msg : String) is
   begin
      if Current_Log_Level <= Debug then
         Log_Message("DEBUG", Msg);
      end if;
   end Debug;

   procedure Info(Msg : String) is
   begin
      if Current_Log_Level <= Info then
         Log_Message("INFO", Msg);
      end if;
   end Info;

   procedure Warning(Msg : String) is
   begin
      if Current_Log_Level <= Warning then
         Log_Message("WARNING", Msg);
      end if;
   end Warning;

   procedure Error(Msg : String) is
   begin
      if Current_Log_Level <= Error then
         Log_Message("ERROR", Msg);
      end if;
   end Error;

end Log;
