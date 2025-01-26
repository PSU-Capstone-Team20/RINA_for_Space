with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Rina;
with System.Win32;

package body RINA_Policies is

Flow_QoS : QoS_Table(1 .. 100);

--QoS dor specific flow defined
procedure Define_QoS(Flow : in Flow_ID; QoS : in QoS_Parameter) is 
begin
	Flow_QoS(Flow) := QoS;
	Put_Line("QoS Defined for Flow " & Integer'Image(Integer(Flow)) & " : Priority= " & Integer'Image(QoS.Priority) & ", Latency=" & Integer'Image(QoS.Latency) & "ms, Throughput=" & Integer'Image(QoS.Throughput) & "kbps");
end Define_QoS;


procedure Schedule_Flow (Flow: in Flow_ID) is
begin
	Put_Line("Scheduling Flow " & Integer'Image(Integer(Flow)) & " with Priorty=" & Integer'Image(Flow_QoS(Flow).Priority));
end Schedule_Flow;

procedure Handle_Error (Flow : in Flow_ID; Error_Code : in Integer) is
begin
	Put_Line("Error on Flow " & Integer'Image(Integer(Flow)) & ": Code " & Integer'Image(Error_Code));
end Handle_Error;

procedure Flow_Control(Flow : in Flow_ID; Max_Packets : in Positive) is
begin
	Put_Line("Controlling Flow " & Integer'Image(Integer(Flow)) & "with max packets" & Positive'Image(Max_Packets));
end Flow_Control;

procedure Relay_And_Forward (Source_Flow : in Flow_ID; Destination_Flow : in Flow_ID) is
begin
	Put_Line("Relaying packets from Flow "  & Integer'Image(Integer(Source_Flow)) & " to Flow " & Integer'Image(Integer(Destination_Flow)));
end Relay_And_Forward;

--for creating new data unit 
procedure Create_Data_Unit(Flow : in Flow_ID; Data_Unit : out Rina.Data_Unit_T) is
begin
   Data_Unit.Configuration.Max_window_Size := 10;
   Data_Unit.Configuration.Timeout := 500;

   Data_Unit.Control_Info := Rina.PCI_T'
                               (Header_Length =>
                                  120,
                                Header        =>
                                  (others => 0),
                                Length        =>
                                  0,
                                DRF_Flag      =>
                                  False,
                                ECN_Flag      =>
                                  True,
                                Src_Addr      =>
                                  (DIF_ID => 1, App_Process_Name => To_Unbounded_String ("Source Application")),
                                Dst_Addr      =>
                                  (DIF_ID => 2, App_Process_Name => To_Unbounded_String ("Destination Application")),
                                Seq_Num       =>
                                  1,
                                QoS_ID        =>
                                  42);

   Put_Line("Created Data Unit for Flow" & Integer'Image(Integer(Flow)));
end Create_Data_Unit;

--process data unit
procedure Process_Data_Unit(Data_Unit : in out Rina.Data_Unit_T) is 
begin
   Data_Unit.Control_Info.Seq_Num := Data_Unit.Control_Info.Seq_Num +1;
   Put_Line("Processed Data Unit. Sequence number incremented to " & Integer'Image(Data_Unit.Control_Info.Seq_Num));
end Process_Data_Unit;

--transmit data unit 
procedure Transmit_Data_Unit(Flow : in Flow_ID; Data_Unit : in Rina.Data_Unit_T) is
begin 
   Put_Line("Transmitting Data unit for flow " & Integer'Image(Integer(Flow)));
   Put_Line("Payload Length: " & Integer'Image(Data_Unit.SDU_Head.Data_Length));
end Transmit_Data_Unit;

-- test: SDNV encoding - converts an integer into an SDNV 
function Encode_SDNV(Value : Integer) return SDNV is
   Result : SDNV (1 .. 5);
   hold : Integer := Value;
   val, bits : Integer;
   num : Byte;
   index : Integer := Result'Last; 
begin
   val := value;
   bits := 0;
   while val /= 0 loop
      bits := bits + 1;
      val := val/128;
   end loop;

   for i in reverse Result'Range loop
      if i = Result'Last then
         Result(i) := 0;
      elsif bits > 0 then
         Result(i) := 128;
      else
         Result(i) := 0;
      end if;
      bits := bits-1;
   end loop;

   while hold /= 0 loop
      val := hold mod 128; 
      num := Byte'Val(Integer'Pos(val));

      Result(index) := Result(index) + num;
      hold := hold / 128; 
      index := index - 1; 
   end loop;

   return Result;
end Encode_SDNV;

-- test: SDNV decoding - converting back to integer 
function Decode_SDNV(SDNV_Value : SDNV) return Integer is 
  result : Integer := 0;
  counter : Integer := 0;
begin
   for i in SDNV_Value'Range loop
      if SDNV_Value(i) /= 0 then
         counter := i;
         exit;
      end if;
   end loop;
  while ((SDNV_Value(counter)) / 128 = 1) loop
    result := result + (Integer'Val(byte'pos(SDNV_Value(counter))) mod 128);
    result := result * 128;
    counter := counter + 1;
  end loop;
    result := result + (Integer'Val(byte'pos(SDNV_Value(counter))) mod 128);
  return result;
end Decode_SDNV;

-- SDNV printing - prints the provided SDNV
procedure Print_SDNV(SDNV_Value : SDNV) is
   counter : Integer := 0;
   quotient : Integer;
   printee : byte;
   divisor : byte;
   print : Unbounded_String;
   firstfound : Boolean := False;
begin
   for i in SDNV_Value'Range loop
      if SDNV_Value(i) /= 0 then
         counter := i;
         exit;
      end if;
   end loop;
   while counter <= SDNV_Value'Last loop
      printee := SDNV_Value(counter);
      divisor := 128;
      while divisor > 0 loop
         quotient := Integer'Val(printee/divisor);
         if divisor = 128 then
            Put(quotient'Image);
            Put(" ");
         else
            if quotient = 1 and not firstfound then
               firstfound := True;
            end if;
            if firstfound then
               Append (print, quotient'Image);
            end if;
            Put(quotient'Image);
         end if;
         printee := printee mod divisor;
         divisor := divisor/2;
      end loop;
      counter := counter + 1;
      Put_Line("");
   end loop;
   Put_Line (print'Image);
end Print_SDNV;

end RINA_Policies;