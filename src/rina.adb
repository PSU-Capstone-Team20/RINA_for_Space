with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with RIB; use RIB;
with Ada.Text_IO; use Ada.Text_IO;

package body RINA is

   

   function Reconstruct_Path return Path_Vectors.Vector is 
      path : Path_Vectors.Vector;
      begin
      return path;
   end Reconstruct_Path;

   function D_Star_Lite (start : Address_Vectors.Vector; goal : Address_Vectors.Vector) return Path_Vectors.Vector is
      OpenList : Node_Vectors.Vector;
      ClosedList : Node_Vectors.Vector;
      node : A_Star_Node;
      current : A_Star_Node;
      startNode : A_Star_Node;
      neighborNode : A_Star_Node;
      map : RIB_Hashed_Maps.map;
      path : Path_Vectors.Vector;
      element : Address_Element;
      OpenListFlag : Integer := 0;
      OpenListIndex : Integer := -1;
      ClosedListFlag : Integer := 0;
   begin
      map := Get_map;
      OpenList.Append(node);
      OpenList(0).Address := goal;
      OpenList(0).gScore := 0;
      OpenList(0).hScore := 0;
      OpenList(0).fScore := OpenList(0).gScore + OpenList(0).hScore;
      current.gScore := -1;
      current.hScore := -1;
      current.fScore := -1;
      startNode.Address := start;
      while OpenList.Is_Empty loop
         --find lowest fScore in the openlist
         for C in OpenList.Iterate loop
            if (current.fScore = -1 or current.fScore > OpenList(C).fScore) then
               current := OpenList(C);
            end if;
         end loop;
         
         --check if we are at target
         if current.Address'Image = startNode.Address'Image then
            path := Reconstruct_Path;
         end if;

         --move current node to closed list
         OpenList.Delete(OpenList.Find_Index(current));
         ClosedList.Append(current);

         --check neighbors of current Node
         --iterate through the difs
         for C in map.Iterate loop
            neighborNode.Address.Append(element);
            neighborNode.Address.Reference(0).Name := map(C).Name;
            neighborNode.Address(0).Address_Type := To_Unbounded_String("DIF");
            --iterate through the Comps
            for X in map(C).Obj_Type.Iterate loop
               neighborNode.Address.Append(element);
               neighborNode.Address.Reference(1).name := map(C).Obj_Type(X).Comp_Connection;
               neighborNode.Address(1).Address_Type := To_Unbounded_String("Computer");
               --iterate through the IPCPs
               for i in map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.Last_Index loop
                  neighborNode.Address.Append(element);
                  neighborNode.Address.Reference(2).Name := map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs(i).IPCP;
                  neighborNode.Address(2).Address_Type := To_Unbounded_String("IPCP");
                  --check if node already exists in either closed or open list
                  for j in OpenList.First_Index .. OpenList.Last_Index loop
                     if (OpenList(j).Contains(neighborNode.Address)) then
                        OpenListFlag := 1;
                        OpenListIndex := j;
                     end if;
                  end loop;
                  for j in ClosedList.First_Index .. ClosedList.Last_Index loop
                     if (ClosedList(j).Contains(neighborNode.Address)) then
                        ClosedListFlag := 1;
                     end if;
                  end loop;
                  if (ClosedListFlag = 0) then
                     neighborNode.gScore := current.gScore + 1;
                     if (OpenListFlag = 0) then
                        OpenList.Prepend(neighborNode);
                     else
                        if(neighborNode.gScore < OpenList(OpenListIndex).gScore) then
                           OpenList(OpenListIndex) := neighborNode;
                        end if;
                     end if;
                  end if;
                  neighborNode.Address.Delete(neighborNode.Address.Last_Index);
                  OpenListFlag := 0;
                  ClosedListFlag := 0;
               end loop;
               --iterate through the APNs
               for i in map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.Last_Index loop
                  neighborNode.Address.Append(element);
                  neighborNode.Address.Reference(3).Name := map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs(i).APN;
                  neighborNode.Address(3).Address_Type := To_Unbounded_String("APN");
                  --check if node already exists in either closed or open list
                  for j in OpenList.First_Index .. OpenList.Last_Index loop
                     if (OpenList(j).Contains(neighborNode.Address)) then
                        OpenListFlag := 1;
                        OpenListIndex := j;
                     end if;
                  end loop;
                  for j in ClosedList.First_Index .. ClosedList.Last_Index loop
                     if (ClosedList(j).Contains(neighborNode.Address)) then
                        ClosedListFlag := 1;
                     end if;
                  end loop;
                  if (ClosedListFlag = 0) then
                     neighborNode.gScore := current.gScore + 1;
                     if (OpenListFlag = 0) then
                        OpenList.Prepend(neighborNode);
                     else
                        if(neighborNode.gScore < OpenList(OpenListIndex).gScore) then
                           OpenList(OpenListIndex) := neighborNode;
                        end if;
                     end if;
                  end if;
                  neighborNode.Address.Delete(neighborNode.Address.Last_Index);
                  OpenListFlag := 0;
                  ClosedListFlag := 0;
               end loop;
               neighborNode.Address.Delete(neighborNode.Address.Last_Index);
            end loop;
            neighborNode.Address.Delete(neighborNode.Address.Last_Index);
         end loop;
      end loop;

      path := Reconstruct_Path;
      return path;
   end D_Star_Lite;




end RINA;