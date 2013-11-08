(DEFINE (PROBLEM CELL-ASSEMBLY-MODEL3B-64)
 (:DOMAIN CELL-ASSEMBLY)
 (:OBJECTS J8 - MACHINE-JOB
           J7 - JOB
           J6 - MACHINE-JOB
           J5 - JOB
           J4 - MACHINE-JOB
           J3 - JOB
           J2 - MACHINE-JOB
           J1 - JOB
           M4 - MACHINE
           M3 - MACHINE
           M2 - MACHINE
           M1 - MACHINE
           TB34 - TABLE
           TB23 - TABLE
           TB12 - TABLE
           T4 - TRAY
           T3 - TRAY
           T2 - TRAY
           T1 - TRAY
           P4 - COMPONENT
           P3 - COMPONENT
           P2 - COMPONENT
           P1 - COMPONENT
           A4 - ARM
           A3 - ARM
           A2 - ARM
           A1 - ARM
           BASE0-2605 - BASE
           BASE1-2604 - BASE
           BASE2-2603 - BASE
           BASE3-2602 - BASE
           BASE4-2601 - BASE
           BASE5-2600 - BASE
           BASE6-2599 - BASE
           BASE7-2598 - BASE
           BASE8-2597 - BASE
           BASE9-2596 - BASE
           BASE10-2595 - BASE
           BASE11-2594 - BASE
           BASE12-2593 - BASE
           BASE13-2592 - BASE
           BASE14-2591 - BASE
           BASE15-2590 - BASE
           BASE16-2589 - BASE
           BASE17-2588 - BASE
           BASE18-2587 - BASE
           BASE19-2586 - BASE
           BASE20-2585 - BASE
           BASE21-2584 - BASE
           BASE22-2583 - BASE
           BASE23-2582 - BASE
           BASE24-2581 - BASE
           BASE25-2580 - BASE
           BASE26-2579 - BASE
           BASE27-2578 - BASE
           BASE28-2577 - BASE
           BASE29-2576 - BASE
           BASE30-2575 - BASE
           BASE31-2574 - BASE
           BASE32-2573 - BASE
           BASE33-2572 - BASE
           BASE34-2571 - BASE
           BASE35-2570 - BASE
           BASE36-2569 - BASE
           BASE37-2568 - BASE
           BASE38-2567 - BASE
           BASE39-2566 - BASE
           BASE40-2565 - BASE
           BASE41-2564 - BASE
           BASE42-2563 - BASE
           BASE43-2562 - BASE
           BASE44-2561 - BASE
           BASE45-2560 - BASE
           BASE46-2559 - BASE
           BASE47-2558 - BASE
           BASE48-2557 - BASE
           BASE49-2556 - BASE
           BASE50-2555 - BASE
           BASE51-2554 - BASE
           BASE52-2553 - BASE
           BASE53-2552 - BASE
           BASE54-2551 - BASE
           BASE55-2550 - BASE
           BASE56-2549 - BASE
           BASE57-2548 - BASE
           BASE58-2547 - BASE
           BASE59-2546 - BASE
           BASE60-2545 - BASE
           BASE61-2544 - BASE
           BASE62-2543 - BASE
           BASE63-2542 - BASE)

 (:INIT
  (ARM-PRESENT TB34)
  (AT A4 TB34)
  (FREE A4)
  (ARM-PRESENT TB23)
  (AT A3 TB23)
  (FREE A3)
  (ARM-PRESENT TB12)
  (AT A2 TB12)
  (FREE A2)
  (ARM-PRESENT TABLE-IN)
  (AT A1 TABLE-IN)
  (FREE A1)
  (= (JOB-COST NOTHING-DONE) 0)
  (JOB-AVAILABLE-AT J1 TB12)
  (DEPENDS NOTHING-DONE J1)
  (= (JOB-COST J1) 3)
  (USES J1 P1)
  (AT P1 T1)
  (JOB-AVAILABLE-AT J2 M1)
  (DEPENDS J1 J2)
  (= (JOB-COST J2) 3)
  (JOB-AVAILABLE-AT J3 TB23)
  (DEPENDS J2 J3)
  (= (JOB-COST J3) 3)
  (USES J3 P2)
  (AT P2 T2)
  (JOB-AVAILABLE-AT J4 M2)
  (DEPENDS J3 J4)
  (= (JOB-COST J4) 2)
  (JOB-AVAILABLE-AT J5 TB34)
  (DEPENDS J4 J5)
  (= (JOB-COST J5) 3)
  (USES J5 P3)
  (AT P3 T3)
  (JOB-AVAILABLE-AT J6 M3)
  (DEPENDS J5 J6)
  (= (JOB-COST J6) 3)
  (JOB-AVAILABLE-AT J7 TB34)
  (DEPENDS J6 J7)
  (= (JOB-COST J7) 2)
  (USES J7 P4)
  (AT P4 T4)
  (JOB-AVAILABLE-AT J8 M4)
  (DEPENDS J7 J8)
  (= (JOB-COST J8) 3)
  (CONNECTED TABLE-OUT CARRY-OUT)
  (CONNECTED CARRY-IN TABLE-IN)
  (= (MOVE-COST TABLE-IN TABLE-IN) 1000)
  (= (MOVE-COST TABLE-IN TB12) 3)
  (= (MOVE-COST TABLE-IN TB23) 5)
  (= (MOVE-COST TABLE-IN TB34) 7)
  (= (MOVE-COST TABLE-IN T4) 8)
  (= (MOVE-COST TABLE-IN M4) 8)
  (= (MOVE-COST TABLE-IN TABLE-OUT) 6)
  (= (MOVE-COST TABLE-IN T3) 7)
  (= (MOVE-COST TABLE-IN M3) 6)
  (= (MOVE-COST TABLE-IN T2) 4)
  (= (MOVE-COST TABLE-IN M2) 4)
  (= (MOVE-COST TABLE-IN T1) 2)
  (= (MOVE-COST TABLE-IN M1) 2)
  (= (MOVE-COST TB12 TABLE-IN) 3)
  (= (MOVE-COST TB12 TB12) 1000)
  (= (MOVE-COST TB12 TB23) 3)
  (= (MOVE-COST TB12 TB34) 5)
  (= (MOVE-COST TB12 T4) 6)
  (= (MOVE-COST TB12 M4) 6)
  (= (MOVE-COST TB12 TABLE-OUT) 4)
  (= (MOVE-COST TB12 T3) 5)
  (= (MOVE-COST TB12 M3) 4)
  (= (MOVE-COST TB12 T2) 2)
  (= (MOVE-COST TB12 M2) 2)
  (= (MOVE-COST TB12 T1) 2)
  (= (MOVE-COST TB12 M1) 3)
  (= (MOVE-COST TB23 TABLE-IN) 5)
  (= (MOVE-COST TB23 TB12) 3)
  (= (MOVE-COST TB23 TB23) 1000)
  (= (MOVE-COST TB23 TB34) 3)
  (= (MOVE-COST TB23 T4) 4)
  (= (MOVE-COST TB23 M4) 4)
  (= (MOVE-COST TB23 TABLE-OUT) 2)
  (= (MOVE-COST TB23 T3) 3)
  (= (MOVE-COST TB23 M3) 2)
  (= (MOVE-COST TB23 T2) 2)
  (= (MOVE-COST TB23 M2) 3)
  (= (MOVE-COST TB23 T1) 4)
  (= (MOVE-COST TB23 M1) 5)
  (= (MOVE-COST TB34 TABLE-IN) 7)
  (= (MOVE-COST TB34 TB12) 5)
  (= (MOVE-COST TB34 TB23) 3)
  (= (MOVE-COST TB34 TB34) 1000)
  (= (MOVE-COST TB34 T4) 2)
  (= (MOVE-COST TB34 M4) 2)
  (= (MOVE-COST TB34 TABLE-OUT) 2)
  (= (MOVE-COST TB34 T3) 3)
  (= (MOVE-COST TB34 M3) 4)
  (= (MOVE-COST TB34 T2) 4)
  (= (MOVE-COST TB34 M2) 5)
  (= (MOVE-COST TB34 T1) 6)
  (= (MOVE-COST TB34 M1) 7)
  (= (MOVE-COST T4 TABLE-IN) 8)
  (= (MOVE-COST T4 TB12) 6)
  (= (MOVE-COST T4 TB23) 4)
  (= (MOVE-COST T4 TB34) 2)
  (= (MOVE-COST T4 T4) 1000)
  (= (MOVE-COST T4 M4) 2)
  (= (MOVE-COST T4 TABLE-OUT) 3)
  (= (MOVE-COST T4 T3) 4)
  (= (MOVE-COST T4 M3) 5)
  (= (MOVE-COST T4 T2) 5)
  (= (MOVE-COST T4 M2) 6)
  (= (MOVE-COST T4 T1) 7)
  (= (MOVE-COST T4 M1) 8)
  (= (MOVE-COST M4 TABLE-IN) 8)
  (= (MOVE-COST M4 TB12) 6)
  (= (MOVE-COST M4 TB23) 4)
  (= (MOVE-COST M4 TB34) 2)
  (= (MOVE-COST M4 T4) 2)
  (= (MOVE-COST M4 M4) 1000)
  (= (MOVE-COST M4 TABLE-OUT) 3)
  (= (MOVE-COST M4 T3) 4)
  (= (MOVE-COST M4 M3) 5)
  (= (MOVE-COST M4 T2) 5)
  (= (MOVE-COST M4 M2) 6)
  (= (MOVE-COST M4 T1) 7)
  (= (MOVE-COST M4 M1) 8)
  (= (MOVE-COST TABLE-OUT TABLE-IN) 6)
  (= (MOVE-COST TABLE-OUT TB12) 4)
  (= (MOVE-COST TABLE-OUT TB23) 2)
  (= (MOVE-COST TABLE-OUT TB34) 2)
  (= (MOVE-COST TABLE-OUT T4) 3)
  (= (MOVE-COST TABLE-OUT M4) 3)
  (= (MOVE-COST TABLE-OUT TABLE-OUT) 1000)
  (= (MOVE-COST TABLE-OUT T3) 2)
  (= (MOVE-COST TABLE-OUT M3) 3)
  (= (MOVE-COST TABLE-OUT T2) 3)
  (= (MOVE-COST TABLE-OUT M2) 4)
  (= (MOVE-COST TABLE-OUT T1) 5)
  (= (MOVE-COST TABLE-OUT M1) 6)
  (= (MOVE-COST T3 TABLE-IN) 7)
  (= (MOVE-COST T3 TB12) 5)
  (= (MOVE-COST T3 TB23) 3)
  (= (MOVE-COST T3 TB34) 3)
  (= (MOVE-COST T3 T4) 4)
  (= (MOVE-COST T3 M4) 4)
  (= (MOVE-COST T3 TABLE-OUT) 2)
  (= (MOVE-COST T3 T3) 1000)
  (= (MOVE-COST T3 M3) 2)
  (= (MOVE-COST T3 T2) 4)
  (= (MOVE-COST T3 M2) 5)
  (= (MOVE-COST T3 T1) 6)
  (= (MOVE-COST T3 M1) 7)
  (= (MOVE-COST M3 TABLE-IN) 6)
  (= (MOVE-COST M3 TB12) 4)
  (= (MOVE-COST M3 TB23) 2)
  (= (MOVE-COST M3 TB34) 4)
  (= (MOVE-COST M3 T4) 5)
  (= (MOVE-COST M3 M4) 5)
  (= (MOVE-COST M3 TABLE-OUT) 3)
  (= (MOVE-COST M3 T3) 2)
  (= (MOVE-COST M3 M3) 1000)
  (= (MOVE-COST M3 T2) 3)
  (= (MOVE-COST M3 M2) 4)
  (= (MOVE-COST M3 T1) 5)
  (= (MOVE-COST M3 M1) 6)
  (= (MOVE-COST T2 TABLE-IN) 4)
  (= (MOVE-COST T2 TB12) 2)
  (= (MOVE-COST T2 TB23) 2)
  (= (MOVE-COST T2 TB34) 4)
  (= (MOVE-COST T2 T4) 5)
  (= (MOVE-COST T2 M4) 5)
  (= (MOVE-COST T2 TABLE-OUT) 3)
  (= (MOVE-COST T2 T3) 4)
  (= (MOVE-COST T2 M3) 3)
  (= (MOVE-COST T2 T2) 1000)
  (= (MOVE-COST T2 M2) 2)
  (= (MOVE-COST T2 T1) 3)
  (= (MOVE-COST T2 M1) 4)
  (= (MOVE-COST M2 TABLE-IN) 4)
  (= (MOVE-COST M2 TB12) 2)
  (= (MOVE-COST M2 TB23) 3)
  (= (MOVE-COST M2 TB34) 5)
  (= (MOVE-COST M2 T4) 6)
  (= (MOVE-COST M2 M4) 6)
  (= (MOVE-COST M2 TABLE-OUT) 4)
  (= (MOVE-COST M2 T3) 5)
  (= (MOVE-COST M2 M3) 4)
  (= (MOVE-COST M2 T2) 2)
  (= (MOVE-COST M2 M2) 1000)
  (= (MOVE-COST M2 T1) 3)
  (= (MOVE-COST M2 M1) 4)
  (= (MOVE-COST T1 TABLE-IN) 2)
  (= (MOVE-COST T1 TB12) 2)
  (= (MOVE-COST T1 TB23) 4)
  (= (MOVE-COST T1 TB34) 6)
  (= (MOVE-COST T1 T4) 7)
  (= (MOVE-COST T1 M4) 7)
  (= (MOVE-COST T1 TABLE-OUT) 5)
  (= (MOVE-COST T1 T3) 6)
  (= (MOVE-COST T1 M3) 5)
  (= (MOVE-COST T1 T2) 3)
  (= (MOVE-COST T1 M2) 3)
  (= (MOVE-COST T1 T1) 1000)
  (= (MOVE-COST T1 M1) 2)
  (= (MOVE-COST M1 TABLE-IN) 2)
  (= (MOVE-COST M1 TB12) 3)
  (= (MOVE-COST M1 TB23) 5)
  (= (MOVE-COST M1 TB34) 7)
  (= (MOVE-COST M1 T4) 8)
  (= (MOVE-COST M1 M4) 8)
  (= (MOVE-COST M1 TABLE-OUT) 6)
  (= (MOVE-COST M1 T3) 7)
  (= (MOVE-COST M1 M3) 6)
  (= (MOVE-COST M1 T2) 4)
  (= (MOVE-COST M1 M2) 4)
  (= (MOVE-COST M1 T1) 2)
  (= (MOVE-COST M1 M1) 1000)
  (REACHABLE A4 M4)
  (REACHABLE A4 TB34)
  (REACHABLE A4 T4)
  (REACHABLE A3 M3)
  (REACHABLE A3 TB34)
  (REACHABLE A3 T3)
  (REACHABLE A3 TABLE-OUT)
  (REACHABLE A3 TB23)
  (REACHABLE A2 M2)
  (REACHABLE A2 TB23)
  (REACHABLE A2 T2)
  (REACHABLE A2 TB12)
  (REACHABLE A1 M1)
  (REACHABLE A1 TB12)
  (REACHABLE A1 T1)
  (REACHABLE A1 TABLE-IN)
  (= (LOADING-COST) 1)
  (= (TOTAL-COST) 0)
  (FINISHED NOTHING-DONE BASE0-2605)
  (FINISHED NOTHING-DONE BASE1-2604)
  (FINISHED NOTHING-DONE BASE2-2603)
  (FINISHED NOTHING-DONE BASE3-2602)
  (FINISHED NOTHING-DONE BASE4-2601)
  (FINISHED NOTHING-DONE BASE5-2600)
  (FINISHED NOTHING-DONE BASE6-2599)
  (FINISHED NOTHING-DONE BASE7-2598)
  (FINISHED NOTHING-DONE BASE8-2597)
  (FINISHED NOTHING-DONE BASE9-2596)
  (FINISHED NOTHING-DONE BASE10-2595)
  (FINISHED NOTHING-DONE BASE11-2594)
  (FINISHED NOTHING-DONE BASE12-2593)
  (FINISHED NOTHING-DONE BASE13-2592)
  (FINISHED NOTHING-DONE BASE14-2591)
  (FINISHED NOTHING-DONE BASE15-2590)
  (FINISHED NOTHING-DONE BASE16-2589)
  (FINISHED NOTHING-DONE BASE17-2588)
  (FINISHED NOTHING-DONE BASE18-2587)
  (FINISHED NOTHING-DONE BASE19-2586)
  (FINISHED NOTHING-DONE BASE20-2585)
  (FINISHED NOTHING-DONE BASE21-2584)
  (FINISHED NOTHING-DONE BASE22-2583)
  (FINISHED NOTHING-DONE BASE23-2582)
  (FINISHED NOTHING-DONE BASE24-2581)
  (FINISHED NOTHING-DONE BASE25-2580)
  (FINISHED NOTHING-DONE BASE26-2579)
  (FINISHED NOTHING-DONE BASE27-2578)
  (FINISHED NOTHING-DONE BASE28-2577)
  (FINISHED NOTHING-DONE BASE29-2576)
  (FINISHED NOTHING-DONE BASE30-2575)
  (FINISHED NOTHING-DONE BASE31-2574)
  (FINISHED NOTHING-DONE BASE32-2573)
  (FINISHED NOTHING-DONE BASE33-2572)
  (FINISHED NOTHING-DONE BASE34-2571)
  (FINISHED NOTHING-DONE BASE35-2570)
  (FINISHED NOTHING-DONE BASE36-2569)
  (FINISHED NOTHING-DONE BASE37-2568)
  (FINISHED NOTHING-DONE BASE38-2567)
  (FINISHED NOTHING-DONE BASE39-2566)
  (FINISHED NOTHING-DONE BASE40-2565)
  (FINISHED NOTHING-DONE BASE41-2564)
  (FINISHED NOTHING-DONE BASE42-2563)
  (FINISHED NOTHING-DONE BASE43-2562)
  (FINISHED NOTHING-DONE BASE44-2561)
  (FINISHED NOTHING-DONE BASE45-2560)
  (FINISHED NOTHING-DONE BASE46-2559)
  (FINISHED NOTHING-DONE BASE47-2558)
  (FINISHED NOTHING-DONE BASE48-2557)
  (FINISHED NOTHING-DONE BASE49-2556)
  (FINISHED NOTHING-DONE BASE50-2555)
  (FINISHED NOTHING-DONE BASE51-2554)
  (FINISHED NOTHING-DONE BASE52-2553)
  (FINISHED NOTHING-DONE BASE53-2552)
  (FINISHED NOTHING-DONE BASE54-2551)
  (FINISHED NOTHING-DONE BASE55-2550)
  (FINISHED NOTHING-DONE BASE56-2549)
  (FINISHED NOTHING-DONE BASE57-2548)
  (FINISHED NOTHING-DONE BASE58-2547)
  (FINISHED NOTHING-DONE BASE59-2546)
  (FINISHED NOTHING-DONE BASE60-2545)
  (FINISHED NOTHING-DONE BASE61-2544)
  (FINISHED NOTHING-DONE BASE62-2543)
  (FINISHED NOTHING-DONE BASE63-2542)
  (AT BASE0-2605 CARRY-IN)
  (AT BASE1-2604 CARRY-IN)
  (AT BASE2-2603 CARRY-IN)
  (AT BASE3-2602 CARRY-IN)
  (AT BASE4-2601 CARRY-IN)
  (AT BASE5-2600 CARRY-IN)
  (AT BASE6-2599 CARRY-IN)
  (AT BASE7-2598 CARRY-IN)
  (AT BASE8-2597 CARRY-IN)
  (AT BASE9-2596 CARRY-IN)
  (AT BASE10-2595 CARRY-IN)
  (AT BASE11-2594 CARRY-IN)
  (AT BASE12-2593 CARRY-IN)
  (AT BASE13-2592 CARRY-IN)
  (AT BASE14-2591 CARRY-IN)
  (AT BASE15-2590 CARRY-IN)
  (AT BASE16-2589 CARRY-IN)
  (AT BASE17-2588 CARRY-IN)
  (AT BASE18-2587 CARRY-IN)
  (AT BASE19-2586 CARRY-IN)
  (AT BASE20-2585 CARRY-IN)
  (AT BASE21-2584 CARRY-IN)
  (AT BASE22-2583 CARRY-IN)
  (AT BASE23-2582 CARRY-IN)
  (AT BASE24-2581 CARRY-IN)
  (AT BASE25-2580 CARRY-IN)
  (AT BASE26-2579 CARRY-IN)
  (AT BASE27-2578 CARRY-IN)
  (AT BASE28-2577 CARRY-IN)
  (AT BASE29-2576 CARRY-IN)
  (AT BASE30-2575 CARRY-IN)
  (AT BASE31-2574 CARRY-IN)
  (AT BASE32-2573 CARRY-IN)
  (AT BASE33-2572 CARRY-IN)
  (AT BASE34-2571 CARRY-IN)
  (AT BASE35-2570 CARRY-IN)
  (AT BASE36-2569 CARRY-IN)
  (AT BASE37-2568 CARRY-IN)
  (AT BASE38-2567 CARRY-IN)
  (AT BASE39-2566 CARRY-IN)
  (AT BASE40-2565 CARRY-IN)
  (AT BASE41-2564 CARRY-IN)
  (AT BASE42-2563 CARRY-IN)
  (AT BASE43-2562 CARRY-IN)
  (AT BASE44-2561 CARRY-IN)
  (AT BASE45-2560 CARRY-IN)
  (AT BASE46-2559 CARRY-IN)
  (AT BASE47-2558 CARRY-IN)
  (AT BASE48-2557 CARRY-IN)
  (AT BASE49-2556 CARRY-IN)
  (AT BASE50-2555 CARRY-IN)
  (AT BASE51-2554 CARRY-IN)
  (AT BASE52-2553 CARRY-IN)
  (AT BASE53-2552 CARRY-IN)
  (AT BASE54-2551 CARRY-IN)
  (AT BASE55-2550 CARRY-IN)
  (AT BASE56-2549 CARRY-IN)
  (AT BASE57-2548 CARRY-IN)
  (AT BASE58-2547 CARRY-IN)
  (AT BASE59-2546 CARRY-IN)
  (AT BASE60-2545 CARRY-IN)
  (AT BASE61-2544 CARRY-IN)
  (AT BASE62-2543 CARRY-IN)
  (AT BASE63-2542 CARRY-IN))
 (:GOAL
  (AND (FINISHED J8 BASE0-2605)
       (FINISHED J8 BASE1-2604)
       (FINISHED J8 BASE2-2603)
       (FINISHED J8 BASE3-2602)
       (FINISHED J8 BASE4-2601)
       (FINISHED J8 BASE5-2600)
       (FINISHED J8 BASE6-2599)
       (FINISHED J8 BASE7-2598)
       (FINISHED J8 BASE8-2597)
       (FINISHED J8 BASE9-2596)
       (FINISHED J8 BASE10-2595)
       (FINISHED J8 BASE11-2594)
       (FINISHED J8 BASE12-2593)
       (FINISHED J8 BASE13-2592)
       (FINISHED J8 BASE14-2591)
       (FINISHED J8 BASE15-2590)
       (FINISHED J8 BASE16-2589)
       (FINISHED J8 BASE17-2588)
       (FINISHED J8 BASE18-2587)
       (FINISHED J8 BASE19-2586)
       (FINISHED J8 BASE20-2585)
       (FINISHED J8 BASE21-2584)
       (FINISHED J8 BASE22-2583)
       (FINISHED J8 BASE23-2582)
       (FINISHED J8 BASE24-2581)
       (FINISHED J8 BASE25-2580)
       (FINISHED J8 BASE26-2579)
       (FINISHED J8 BASE27-2578)
       (FINISHED J8 BASE28-2577)
       (FINISHED J8 BASE29-2576)
       (FINISHED J8 BASE30-2575)
       (FINISHED J8 BASE31-2574)
       (FINISHED J8 BASE32-2573)
       (FINISHED J8 BASE33-2572)
       (FINISHED J8 BASE34-2571)
       (FINISHED J8 BASE35-2570)
       (FINISHED J8 BASE36-2569)
       (FINISHED J8 BASE37-2568)
       (FINISHED J8 BASE38-2567)
       (FINISHED J8 BASE39-2566)
       (FINISHED J8 BASE40-2565)
       (FINISHED J8 BASE41-2564)
       (FINISHED J8 BASE42-2563)
       (FINISHED J8 BASE43-2562)
       (FINISHED J8 BASE44-2561)
       (FINISHED J8 BASE45-2560)
       (FINISHED J8 BASE46-2559)
       (FINISHED J8 BASE47-2558)
       (FINISHED J8 BASE48-2557)
       (FINISHED J8 BASE49-2556)
       (FINISHED J8 BASE50-2555)
       (FINISHED J8 BASE51-2554)
       (FINISHED J8 BASE52-2553)
       (FINISHED J8 BASE53-2552)
       (FINISHED J8 BASE54-2551)
       (FINISHED J8 BASE55-2550)
       (FINISHED J8 BASE56-2549)
       (FINISHED J8 BASE57-2548)
       (FINISHED J8 BASE58-2547)
       (FINISHED J8 BASE59-2546)
       (FINISHED J8 BASE60-2545)
       (FINISHED J8 BASE61-2544)
       (FINISHED J8 BASE62-2543)
       (FINISHED J8 BASE63-2542)
       (AT BASE0-2605 CARRY-OUT)
       (AT BASE1-2604 CARRY-OUT)
       (AT BASE2-2603 CARRY-OUT)
       (AT BASE3-2602 CARRY-OUT)
       (AT BASE4-2601 CARRY-OUT)
       (AT BASE5-2600 CARRY-OUT)
       (AT BASE6-2599 CARRY-OUT)
       (AT BASE7-2598 CARRY-OUT)
       (AT BASE8-2597 CARRY-OUT)
       (AT BASE9-2596 CARRY-OUT)
       (AT BASE10-2595 CARRY-OUT)
       (AT BASE11-2594 CARRY-OUT)
       (AT BASE12-2593 CARRY-OUT)
       (AT BASE13-2592 CARRY-OUT)
       (AT BASE14-2591 CARRY-OUT)
       (AT BASE15-2590 CARRY-OUT)
       (AT BASE16-2589 CARRY-OUT)
       (AT BASE17-2588 CARRY-OUT)
       (AT BASE18-2587 CARRY-OUT)
       (AT BASE19-2586 CARRY-OUT)
       (AT BASE20-2585 CARRY-OUT)
       (AT BASE21-2584 CARRY-OUT)
       (AT BASE22-2583 CARRY-OUT)
       (AT BASE23-2582 CARRY-OUT)
       (AT BASE24-2581 CARRY-OUT)
       (AT BASE25-2580 CARRY-OUT)
       (AT BASE26-2579 CARRY-OUT)
       (AT BASE27-2578 CARRY-OUT)
       (AT BASE28-2577 CARRY-OUT)
       (AT BASE29-2576 CARRY-OUT)
       (AT BASE30-2575 CARRY-OUT)
       (AT BASE31-2574 CARRY-OUT)
       (AT BASE32-2573 CARRY-OUT)
       (AT BASE33-2572 CARRY-OUT)
       (AT BASE34-2571 CARRY-OUT)
       (AT BASE35-2570 CARRY-OUT)
       (AT BASE36-2569 CARRY-OUT)
       (AT BASE37-2568 CARRY-OUT)
       (AT BASE38-2567 CARRY-OUT)
       (AT BASE39-2566 CARRY-OUT)
       (AT BASE40-2565 CARRY-OUT)
       (AT BASE41-2564 CARRY-OUT)
       (AT BASE42-2563 CARRY-OUT)
       (AT BASE43-2562 CARRY-OUT)
       (AT BASE44-2561 CARRY-OUT)
       (AT BASE45-2560 CARRY-OUT)
       (AT BASE46-2559 CARRY-OUT)
       (AT BASE47-2558 CARRY-OUT)
       (AT BASE48-2557 CARRY-OUT)
       (AT BASE49-2556 CARRY-OUT)
       (AT BASE50-2555 CARRY-OUT)
       (AT BASE51-2554 CARRY-OUT)
       (AT BASE52-2553 CARRY-OUT)
       (AT BASE53-2552 CARRY-OUT)
       (AT BASE54-2551 CARRY-OUT)
       (AT BASE55-2550 CARRY-OUT)
       (AT BASE56-2549 CARRY-OUT)
       (AT BASE57-2548 CARRY-OUT)
       (AT BASE58-2547 CARRY-OUT)
       (AT BASE59-2546 CARRY-OUT)
       (AT BASE60-2545 CARRY-OUT)
       (AT BASE61-2544 CARRY-OUT)
       (AT BASE62-2543 CARRY-OUT)
       (AT BASE63-2542 CARRY-OUT)))
 (:METRIC MINIMIZE (TOTAL-COST)))