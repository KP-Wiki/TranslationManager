unit KM_CommonTypes;
{$I KM_CompilerDirectives.inc}
interface

  // Todo items classification

  //todo -cAssets: _Game assets are needed (sprites, sounds, etc.)
  //todo -cComplicated: _Likely required, but is highly complicated to implement
  //todo -cFeature*: _Feature name
  //todo -cOptimization: _Likely can be optimized
  //todo -cPractical: _Something that just needs to be done
  //todo -cQuestionable: _Needs to be evaluated first, maybe it is needed, maybe not, maybe

// Common types non-specific for the game
type
  TKMByteArray = array of Byte;
  TKMWordArray = array of Word;
  TKMWordArray4 = array [0..3] of Word;
  PKMWordArray = ^TKMWordArray;
  TKMCardinalArray = array of Cardinal;
  TKMSingleArray = array of Single;
  TIntegerArray = array of Integer;
  TStringArray = TArray<string>;
  TByteSet = set of Byte;

  (*
  TKMEvent
  TKMEventRef
  TKMEventObjectInteger2
  TKMEventSingle
  *)

  TKMEvent = procedure of object;
  TKMPointEvent = procedure (Sender: TObject; const X,Y: Integer) of object;

  // Reference to procedure const
  TKMProcConst<T> = reference to procedure (const Arg1: T);
  TKMProcConst<T1, T2> = reference to procedure (const Arg1: T1; const Arg2: T2);

  // Procedure of object
  TKMEvent<T> = procedure (Arg1: T) of object;
  TKMEvent<T1, T2> = procedure (Arg1: T1; Arg2: T2) of object;

  // Procedure of object const
  TKMEventConst<T> = procedure (const Arg1: T) of object;
  TKMEventConst<T1, T2> = procedure (const Arg1: T1; const Arg2: T2) of object;

  TKMIntegerStringEvent = procedure (aValue: Integer; const aText: string) of object;

  TKMResyncEvent = procedure (aSender: Integer; aTick: Cardinal) of object;

implementation


end.

