
----------------------------------------------------------
--  This three-task program includes the main procedure,
--  helloadatasks, as the "main task" and two tasks, Task_A and
--  Task_B, nested inside it.
----------------------------------------------------------

with Ada.Text_IO, Ada.Calendar;
use  Ada.Text_IO, Ada.Calendar;

procedure helloadatasks is

  Start_Time   : constant Time := Clock;
  Elapsed_Time : Duration;

  task Task_A is
    entry Go;
  end Task_A;

  task Task_B is
    entry Go;
  end Task_B;

  task body Task_A is
  begin
    delay 2.0;                           -- relative delay
    accept Go;
    Elapsed_Time := Clock - Start_Time;
    Put_Line("helloadatasks/Task_A Rendezvous occurred at T = "
             & Integer'Image(Integer(Elapsed_Time)));
    for I in 1..5 loop
      delay 2.0;                         -- relative delay
      Task_B.Go;
    end loop;
  end Task_A;

  task body Task_B is
  begin
    for I in 1..5 loop
      delay until (Start_Time + I*7.0);  --absolute delay
      accept Go;
      Elapsed_Time := Clock - Start_Time;
      Put_Line("Task_A/Task_B Rendezvous occurred at T = "
               & Integer'Image(Integer(Elapsed_Time)));
    end loop;
  end Task_B;

begin

  Task_A.Go;

end helloadatasks;

