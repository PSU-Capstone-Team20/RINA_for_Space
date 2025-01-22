with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;
with Rina;

package body RINA_Policies is

  --encoding logic for SDNV
  function Encode_SDNV(Value : Integer) return SDNV is
    Temp    : Integer  := Value;
    Result  : SDNV(1 .. 10);
    Index   : Positive := Result'Last;
  begin
    if Value < 0 then 
      raise Constraint_Error;
    end if;

    loop
      Result(Index) := Interfaces.Unsigned_8(Temp mod 128);
      if Index < Result'First then 
        raise Constraint_Error;
      end if;

      Temp := Temp / 128;

      if Temp > 0 then 
        Result(Index) := Result(Index) or 128;
      end if;

      Index := Index - 1;

      exit when Temp = 0;
    end loop;

    return Result(Index + 1 .. Result'Last);
  end Encode_SDNV;

  --decode SDNV logic 
  function Decode_SDNV(SDNV_Value : SDNV) return Integer is 
    Result   : Integer  := 0;
    Shift    : Integer  := 0;

  begin
    for Byte_Value of SDNV_Value loop
      Result := Result + ((Byte_Value and 127) * (2 ** Shift));
      if (Byte_Value and 128) = 0 then 
        return Result;
      end if;
      Shift := Shift + 7;
    end loop;

    raise Constraint_Error;
  end Decode_SDNV;

  --encode QoS to SDNV
  function Encode_QoS(QoS : Rina.QoS_Parameter) return SDNV is 
    Priority_En      : SDNV  := Encode_SDNV(QoS.Priority);
    Latency_En       : SDNV  := Encode_SDNV(QoS.Latency);
    Throughput_En    : SDNV  := Encode_SDNV(QoS.Throughput);
  begin
    return Priority_En & Latency_En & Throughput_En;
  end Encode_QoS;

  -- decode QoS SDNV
  function Decode_QoS(SDNV_Value : SDNV) return Rina.QoS_Parameter is 
    Priority        : SDNV  := Decode_SDNV(SDNV_Value(1 .. 2)); 
    Latency         : SDNV  := Decode_SDNV(SDNV_Value(3 ..4));
    Throughput      : SDNV  := Decode_SDNV(SDNV_Value(5 ..6));
  begin 
    return (Priority => Priority, Latency => Latency, Throughput => Throughput);
  end Decode_QoS;

  --encode Endpoint ID to SDNV
  function Encode_Endpoint_ID(Endpoint : Rina.Endpoint_ID) return SDNV is 
    ID_En          : SDNV  := Encode_SDNV(Endpoint.ID);
    Name_En        : SDNV  := To_String(Endpoint.App_Process_Name);
  begin
    return ID_En;
  end Encode_Endpoint_ID;

  --decode Endpoint SDNV 
  function Decode_Endpoint_ID(SDNV_Value : SDNV) return Rina.Endpoint_ID is 
    ID_Decode      : Integer  := Decode_SDNV(SDNV_Value(1 .. 2));
  begin 
    return (App_Process_name => To_Unbounded_String("Placeholder"),
            ID => ID_Decode);
  end Decode_Endpoint_ID;

  --creating new data unit 
  procedure Create_Data_Unit(Flow         : Flow_ID;
                             QoS          : Rina.QoS_Parameter;
                             Source       : Rina.Endpoint_ID;
                             Destination  : Rina.Endpoint_ID;
                             Unit         : out Rina.Data_Unit_T) is 
  begin
    -- assign Flow ID
    Unit.Control_Info.Src_Endpoint := Source;
    Unit.Control_Info.Dst_Endpoint := Destination;
    Unit.Control_Info.QoS := QoS;

   -- initialize other control information fields
    Unit.Control_Info.Seq_Num := 0; -- Initial sequence number
    Unit.Control_Info.PDU_Type := "Data Transfer"; -- Example PDU type

   -- Placeholder configuration and payload
    Unit.Configuration.Max_Window_Size := 10;
    Unit.Configuration.Timeout := 1000; -- in milliseconds
    Unit.SDU_Head.Data_Length := 0; -- No payload initially
    Unit.SDU_Tail.Data_Length := 0;
  end Create_Data_Unit;

  --encoding data unit 
  function Encode_Data_Unit(Unit : Data_Unit) return SDNV is 
    Flow_En         : SDNV  := Encode_SDNV(Integer(Unit.Flow));
    QoS_En          : SDNV  := Encode_QoS(Unit.QoS);
    Source_En       : SDNV  := Encode_Endpoint_ID(Unit.Source);
    Destination_En  : SDNV  := Encode_Endpoint_ID(Unit.Destination);
  begin
    return Flow_En & QoS_En & Source_En & Destination_En;
  end Encode_Data_Unit;

  --decode data unit SDNV 
  function Decode_Data_Unit(SDNV_Value : SDNV) return Data_Unit is 
    Flow          : Flow_ID       := Flow_ID(Decode_SDNV(SDNV_Value(1 .. 2)));
    QoS           : Rina.QoS_Parameter := Decode_QoS(SDNV_Value(3 .. 6));
    Source        : Rina.Endpoint_ID   := Decode_Endpoint_ID(SDNV_Value(7 .. 8));
    Destination   : Rina.Endpoint_ID   := Decode_Endpoint_ID(SDNV_Value(9 .. 10));
  begin
    return (Flow => Flow, QoS => QoS, Source => Source, Destination => Destination);
  end Decode_Data_Unit;

  procedure Process_Data_Unit(Unit : in out Rina.Data_Unit_T) is
  begin
    -- Increment the sequence number
    Unit.Control_Info.Seq_Num := Unit.Control_Info.Seq_Num + 1;
    Put_Line("Processed Data Unit: Sequence number incremented to " &
            Integer'Image(Unit.Control_Info.Seq_Num));
  end Process_Data_Unit;
 
  procedure Transmit_Data_Unit(Flow : Flow_ID; Unit : in Rina.Data_Unit_T) is
  begin
    Put_Line("Transmitting Data Unit for Flow " & Integer'Image(Integer(Flow)));
    Put_Line(" Source Endpoint: " & To_String(Unit.Control_Info.Src_Endpoint.App_Process_Name));
    Put_Line(" Destination Endpoint: " & To_String(Unit.Control_Info.Dst_Endpoint.App_Process_Name));
    Put_Line(" Payload Length: " & Integer'Image(Unit.SDU_Head.Data_Length));
  end Transmit_Data_Unit;

  --logging data info 
  procedure Log_Data_Unit(Unit : Data_Unit) is 
  begin
    Put_Line("Data Unit Transfer");
    Put_Line(" Flow ID: " & Integer'Image(Integer(Unit.Flow)));
    Put_Line(" QoS: Priority:" & Integer'Image(Unit.QoS.Priority) & 
             ", Latency:" & Integer'Image(Unit.QoS.Latency) &
             ", Throughput:" & Integer'Image(Unit.QoS.Throughput));
    Put_Line(" Source: " & To_String(Unit.Source.App_Process_Name));
    Put_Line(" Destination: " & To_String(Unit.Destination.App_Process_Name));
  end Log_Data_Unit;

end RINA_Policies;