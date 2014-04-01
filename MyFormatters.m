(* ::Package:: *)

(*
Copyright (c) 2014, Dean Thompson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

On[Assert]; 

ClearAll["MyFormatters`*"];
ClearAll["MyFormatters`Private`*"];

BeginPackage["MyFormatters`"];

FormatAsPercent::usage =
  "FormatAsPercent[fraction, digitsAfterDecimal_Integer] formats fraction " <> 
  "as a percentage with the specified number of digits after the decimal " <>
  "and with a trailing percent sign.";

PercentTicks::usage =
  "Suitable ticks function for graph axes that are fractions which should be " <>
  "interpreted as percentages. PercentTicks[min_, max_] generates an appropriate " <>
  "list of ticks, where each tick has the form { value, label }.";

Begin["`Private`"];

FormatAsPercent[fraction_, digitsAfterDecimal_Integer: 0] := 
  Module[{s},
  s = ToString@NumberForm[N[100*fraction], 
        {digitsAfterDecimal+3, digitsAfterDecimal}];
  s = StringReplace[s, "." ~~ EndOfString -> ""];
  s <> "%"];

Assert[FormatAsPercent[0.344] == "34%"];
Assert[FormatAsPercent[0.344, 0] == "34%"];
Assert[FormatAsPercent[0.344, 1] == "34.4%"];
Assert[FormatAsPercent[0.344, 2] == "34.40%"];
Assert[FormatAsPercent[1.50, 2] == "150.00%"];

(* Display tick marks as percentages. *)
ClearAll[PercentTicks];
PercentTicks[min_, max_] := Module[
  {
    targetNumLabels = 5,
    targetLabelInterval, targetLabelExp, targetLabelMantessa, 
    labelMantessa, labelInterval,
    ticksPerLabel, tickInterval, firstTickNum, lastTickNum,
    digitsAfterDecimal
  },
  targetLabelInterval = (max - min) / targetNumLabels;
  targetLabelExp = Floor[Log[10, targetLabelInterval]];
  digitsAfterDecimal = Max[0, -2 - targetLabelExp];
  targetLabelMantessa = targetLabelInterval / 10^targetLabelExp;
  labelMantessa = Which[
    targetLabelMantessa < 1.5, 1,
    targetLabelMantessa < 3.5, 2,
    targetLabelMantessa < 7.5, 5, 
    True, 10];
  labelInterval = labelMantessa * 10^targetLabelExp;
  ticksPerLabel = Switch[labelMantessa,
    1, 5,
    2, 2,
    5, 5,
    10, 5];
  tickInterval = labelInterval / ticksPerLabel;
  firstTickNum = Ceiling[min / tickInterval];
  lastTickNum = Floor[max / tickInterval];
  Table[{tickNum * tickInterval,
           If[Mod[tickNum, ticksPerLabel] == 0,
                FormatAsPercent[tickNum * tickInterval,
                                                  digitsAfterDecimal],
               ""]},
           { tickNum, firstTickNum, lastTickNum }]
];

End[ ];
EndPackage[ ];


(* Example.
ListPlot[Table[{x, x}, {x, 0, .003, 0.0001}], 
         Ticks -> { Automatic, PercentTicks }]
 *)
