with Ada.Containers.Vectors;
with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Flow_Manager; use Flow_Manager;
with Res_Manager; use Res_Manager;

package IPC_API is

   -- Port ID Type
   type Port_ID is new Integer;
   type QoS_Level is (Low, Medium, High);

   -- Container for Active Flows
   package Flow_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Flow_Record);

   -- IPC API Functions
   function Allocate(Source : String; Destination : String; QoS : QoS_Level) return Port_ID;
   procedure Send(Port : Port_ID; Data : String);
   function Receive(Port : Port_ID) return String;
   procedure Deallocate(Port : Port_ID);

end IPC_API;
