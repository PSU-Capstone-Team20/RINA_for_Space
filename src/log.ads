package Log is
   type Log_Level_T is (Debug, Info, Warning, Error, Silent);

   Current_Log_Level : Log_Level_T := Info;

   procedure Debug(Msg : String);
   procedure Info(Msg : String);
   procedure Warning(Msg : String);
   procedure Error(Msg : String);

end Log;
