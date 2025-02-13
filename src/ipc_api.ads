with Ada.Containers.Vectors;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Flow_Manager; use Flow_Manager;
with Res_Manager; use Res_Manager;

package IPC_API is

   -- Port ID Type
   type Port_ID is new Integer;
   type QoS_Level is range 0 .. 2; -- 0 = Low, 1 = Medium, 2 = High

   -- Container for Active Flows
   package Flow_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Flow_Record);

   -- IPC API Functions
   function Request_Flow(Source : String; Destination : String; QoS : QoS_Level) return Port_ID;
   procedure Transmit_Data_To_Dst(Port : Port_ID; Data : String); -- SDU Transmission
   function Receive_SDU(Port : Port_ID) return String;
   procedure Deallocate_Flow(Port : Port_ID); -- Terminates flow and free allocated resources

end IPC_API;
