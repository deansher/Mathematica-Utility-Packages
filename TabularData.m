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

ClearAll["TabularData`*"];
ClearAll["TabularData`Private`*"];

BeginPackage["TabularData`"];

MergeTallies::usage =
  "MergeTallies[tallies_List] expects each row of tallies to be a list " <>
  "with a count as its last entry. All but the last entry of each row are " <>
  "taken to be labels (of any data type). MergeTallies returns a new list " <>
  "rows in the same format but with total counts for each unique " <> 
  "combination of labels. The rows are sorted by the labels.";

TalliesToFractions::usage =
  "TalliesToFractions[tallies_List] expects each row of tallies to be a list " <>
  "with a count as its last entry. It returns a new list of rows in the same " <>
  "format but with the counts replaced by fractions (count / total count).";

OuterJoin::usage =
  "OuterJoin[left_?MatrixQ, right_?MatrixQ] expects some columns of " <>
  "left and right to be keys while the others are values. " <>
  "It combines rows from left and right that have matching keys." <>
  "The output is a matrix comprised of a single copy of the key " <>
  "columns, then the value columns from left, and then the value columns " <>
  "from right.\n\n" <>
  "It is an error for either left or right to contain multiple rows with " <>
  "the same keys.\n\n" <>
  "If a combination of keys occcurs in left or right but not both, the " <>
  "output contains a row for those keys and the missing columns receive " <>
  "a default value, which is Missing[] unless otherwise specified by the " <>
  "\"MissingValue\" option.\n\n" <> 
  "Note, however, that this function is sadly unable to handle the case " <>
  "where either left or right has no rows, because the empty list is not " <>
  "a matrix, and because in any case this function would then not know " <>
  "how many values to regard as missing.\n\n" <>
  "By default, the first column of each input is " <>
  "the key column, but this can be overridden by providing the " <>
  "\"LeftKeyColumns\" or \"RightKeyColumns\" options, which are each " <>
  "a list of integers giving the key column numbers. ";

Begin["`Private`"];

MergeTallies[tallies_List] := Module[{grouped},
  grouped = GatherBy[tallies, Most];
  Sort[(Most@First@# ~Append~ Total[Last /@ #] &) /@ grouped]];

Assert[MergeTallies@{{"a", "b", 3}, {"a", "b", 4}, {"a", "c", 2}} ==
       {{"a", "b", 7}, {"a", "c", 2}}];

TalliesToFractions[tallies_List] := Module[{total},
  total = Total[tallies[[All, -1]]];
  (Append[Most[#], Last[#]/total] &) /@ tallies];

Assert[TalliesToFractions[{{"a", "b", 1}, {"a", "c", 3}}] ==
  {{"a", "b", 1/4}, {"a", "c", 3/4}}];

Options[OuterJoin] = {
  "MissingValue" -> Missing[],
  "LeftKeyColumns" -> {1},
  "RightKeyColumns" -> {1}
};

OuterJoin::invalidkeycolumns = "Invalid key column lists `1` and `2`";
OuterJoin::duplicatekeys = "Duplicate keys on the `1`: `2`";

OuterJoin[xrows_?MatrixQ, yrows_?MatrixQ, opts: OptionsPattern[]] := 
  Module[{kx, ky, missing,
          xWidth, numXVals, vx, yWidth, numYVals, vy,
          X, Y, xs, ys, groups, join},
    kx = OptionValue["LeftKeyColumns"];
    ky = OptionValue["RightKeyColumns"];

    If[!MatchQ[kx, {_Integer..}] || !MatchQ[ky, {_Integer..}] || 
       Length@kx != Length@ky,
      Message[OuterJoin::invalidkeycolumns, kx, ky]; Return[]];

    missing = OptionValue["MissingValue"];

    xWidth = Length@First@xrows;
    numXVals = xWidth - Length@kx;
    vx = Complement[Range@xWidth, kx];

    yWidth = Length@First@yrows;
    numYVals = yWidth - Length@ky;
    vy = Complement[Range@yWidth, ky];

    xs = (X[#[[kx]], #[[vx]]] &) /@ xrows;
    ys = (Y[#[[ky]], #[[vy]]] &) /@ yrows;

    groups = Join[xs, ys] ~GatherBy~ First;

    join[g_] := Module[{gxs = Cases[g, _X], gys = Cases[g, _Y], 
                        keys, xVals, yVals},
      If[Length@gxs > 1, 
        Message[OuterJoin::duplicatekeys, "left", gxs[[1, 1]]]];
      If[Length@gys > 1,
        Message[OuterJoin::duplicatekeys, "right", gys[[1, 1]]]];

      keys = If[Length@gxs > 0, gxs[[1, 1]], gys[[1, 1]]];
      xVals = If[Length@gxs > 0, gxs[[1, 2]], Table[missing, {numXVals}]];
      yVals = If[Length@gys > 0, gys[[1, 2]], Table[missing, {numYVals}]];
      Join[keys, xVals, yVals]
    ];
    join /@ groups
];

Assert[Sort@OuterJoin[({
 {x, 1},
 {z, 3}
}), ({
 {x, "x"},
 {y, "y"}
})] == 
       Sort[{
 {x, 1, "x"},
 {y, Missing[], "y"},
 {z, 3, Missing[]}
}]] ;

End[ ];
EndPackage[ ];

