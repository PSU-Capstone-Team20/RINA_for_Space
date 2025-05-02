with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with RIB; use RIB;
with Ada.Text_IO; use Ada.Text_IO;

package body RINA is

   

   function Reconstruct_Path (ClosedList : Node_Vectors.Vector; current : out A_Star_Node) return Path_Vectors.Vector is 
      path     : Path_Vectors.Vector;
      begin
         --add the first address to the path
         path.Append (current.Address);
         -- look for the parent node in the closed list by matching its address to the current node's address
         for I in reverse ClosedList.First_Index .. ClosedList.Last_Index loop
            if Address_Vectors."=" (ClosedList(I).Address, current.parentAddress) then
               --set current node as goal node 
               current := ClosedList(I);
               --add the current address to the path
               path.Append (current.Address);
               --check if the next address in the chain exists, if not break out of loop before accessing null
               if current.parentAddress.Is_Empty then
                  exit;
               end if;
            end if;
         end loop;
         --return result
         return path;
   end Reconstruct_Path;
   
   --call this when either a new message is being sent, or if an obstacle was encounter, it will function off the current existing RIB
   --assumed to be O(nlogn)
   --TODO: needs priority weights from DIFs to properly compute best path, currently the function is indistinguishable from Dijkstra's algorithm
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
      --acquire map for pathing
      map := Get_map;
      --start the checking list
      OpenList.Append(node);
      OpenList(0).Address := goal;
      OpenList(0).gScore := 0;
      OpenList(0).hScore := 0;
      OpenList(0).fScore := OpenList(0).gScore + OpenList(0).hScore;
      --assign initial purposeful fail values
      current.gScore := -1;
      current.hScore := -1;
      current.fScore := -1;
      --set target
      startNode.Address := start;
      --begin pathfinding
      while not (OpenList.Is_Empty) loop
         --find lowest fScore in the openlist
         current := OpenList(0);
         for C in OpenList.Iterate loop
            if (current.fScore = -1 or current.fScore > OpenList(C).fScore) then
               current := OpenList(C);
            end if;
         end loop;
         
         --check if we are at target
         if current.Address'Image = startNode.Address'Image then
            path := Reconstruct_Path(ClosedList, current);
            return path;
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
               --check for neighborliness
               if (current.Address(0) = neighborNode.Address(0) or current.Address(1) = neighborNode.Address(1)) then
                  --only proceed if either the computer is the same or the current node is an ipcp
                  if(current.Address(1).Name = neighborNode.Address(1).Name or current.Address(2).Address_Type = To_Unbounded_String ("IPCP")) then
                     --iterate through the IPCPs
                     for i in map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs.Last_Index loop
                        neighborNode.Address.Append(element);
                        neighborNode.Address.Reference(2).Name := map(C).Obj_Type(X).Obj_Obj_Type.Accessible_IPCPs(i);
                        neighborNode.Address(2).Address_Type := To_Unbounded_String("IPCP");
                        --check if node already exists in the open list
                        for j in OpenList.First_Index .. OpenList.Last_Index loop
                           if Address_Vectors."=" (OpenList(j).Address, neighborNode.Address) then
                              OpenListFlag := 1;
                              OpenListIndex := j; --save index for later comparison
                           end if;
                        end loop;
                        --check if node already exists in the closed list
                        for j in ClosedList.First_Index .. ClosedList.Last_Index loop
                           if Address_Vectors."=" (ClosedList(j).address, neighborNode.Address) then
                              ClosedListFlag := 1;
                           end if;
                        end loop;
                        --if in closed list, abort
                        if (ClosedListFlag = 0) then
                           neighborNode.parentAddress := current.Address;
                           neighborNode.gScore := current.gScore + 1;
                           neighborNode.hScore := 0;
                           neighborNode.fScore := neighborNode.gScore + neighborNode.hScore;
                           --if in open list compare scores and take the lowest
                           if (OpenListFlag = 0) then
                              OpenList.Prepend(neighborNode);
                           else
                              if(neighborNode.gScore < OpenList(OpenListIndex).gScore) then
                                 OpenList(OpenListIndex) := neighborNode;
                              end if;
                           end if;
                        end if;
                        --delete the last index of the address and reset the flags
                        neighborNode.Address.Delete(neighborNode.Address.Last_Index);
                        OpenListFlag := 0;
                        ClosedListFlag := 0;
                     end loop;
                  end if;
                  --only proceed if the computer is the same
                  if (current.Address(1).Name = neighborNode.Address(1).Name) then
                     --iterate through the APNs
                     for i in map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.First_Index .. map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs.Last_Index loop
                        neighborNode.Address.Append(element);
                        neighborNode.Address.Reference(2).Name := map(C).Obj_Type(X).Obj_Obj_Type.Active_APNs(i);
                        neighborNode.Address(2).Address_Type := To_Unbounded_String("APN");
                        --check if node already exists in the open list
                        for j in OpenList.First_Index .. OpenList.Last_Index loop
                           if Address_Vectors."=" (OpenList(j).Address, neighborNode.Address) then
                              OpenListFlag := 1;
                              OpenListIndex := j; --save index for later comparison
                           end if;
                        end loop;
                        --check if node already exists in the closed list
                        for j in ClosedList.First_Index .. ClosedList.Last_Index loop
                           if Address_Vectors."=" (ClosedList(j).address, neighborNode.Address) then
                              ClosedListFlag := 1;
                           end if;
                        end loop;
                        --if in closed list, abort
                        if (ClosedListFlag = 0) then
                           neighborNode.parentAddress := current.Address;
                           neighborNode.gScore := current.gScore + 1;
                           neighborNode.hScore := 0;
                           neighborNode.fScore := neighborNode.gScore + neighborNode.hScore;
                           --if in open list compare scores and take the lowest
                           if (OpenListFlag = 0) then
                              OpenList.Prepend(neighborNode);
                           else
                              if(neighborNode.gScore < OpenList(OpenListIndex).gScore) then
                                 OpenList(OpenListIndex) := neighborNode;
                              end if;
                           end if;
                        end if;
                        --delete the last index of the address and reset the flags
                        neighborNode.Address.Delete(neighborNode.Address.Last_Index);
                        OpenListFlag := 0;
                        ClosedListFlag := 0;
                     end loop;
                  end if;
               end if;
               neighborNode.Address.Delete(neighborNode.Address.Last_Index);
            end loop;
            neighborNode.Address.Delete(neighborNode.Address.Last_Index);
         end loop;
      end loop;
      
      return path;
   end D_Star_Lite;

end RINA;