// WDC/FPC defines are used because Delphi/Lazarus have some RTL differences (mostly in uses sections)
//----------------------------------------------------------------------------------------------------
// If you use any Delphi compiler that is not listed here - you need to add it
{$IFDEF VER220} {$DEFINE WDC} {$ENDIF} // Delphi XE
{$IFDEF VER230} {$DEFINE WDC} {$ENDIF} // Delphi XE2
{$IFDEF VER240} {$DEFINE WDC} {$ENDIF} // Delphi XE3
{$IFDEF VER250} {$DEFINE WDC} {$ENDIF} // Delphi XE4
{$IFDEF VER260} {$DEFINE WDC} {$ENDIF} // Delphi XE5
{$IFDEF VER270} {$DEFINE WDC} {$ENDIF} // Delphi XE6
{$IFDEF VER280} {$DEFINE WDC} {$ENDIF} // Delphi XE7
{$IFDEF VER290} {$DEFINE WDC} {$ENDIF} // Delphi XE8
{$IFDEF VER300} {$DEFINE WDC} {$ENDIF} // Delphi 10 Seattle - untested
{$IFDEF VER310} {$DEFINE WDC} {$ENDIF} // Delphi 10.1 Berlin - untested
{$IFDEF VER320} {$DEFINE WDC} {$ENDIF} // Delphi 10.2 Tokyo - untested
{$IFDEF VER330} {$DEFINE WDC} {$ENDIF} // Delphi 10.3 Rio - untested
{$IFDEF VER340} {$DEFINE WDC} {$ENDIF} // Delphi 10.4 Sydney
{$IFDEF VER350} {$DEFINE WDC} {$ENDIF} // Delphi 11 Alexandria
{$IFDEF VER360} {$DEFINE WDC} {$ENDIF} // Delphi 12
{.$DEFINE FPC} // Define Free Pascal compiler (defined automatically by Lazarus)


// Lazarus compiler specific settings
//----------------------------------------------------------------------------------------------------
{$IFDEF FPC}
  // Set FPC to work in Delphi mode
  {$MODE Delphi}

  // Currently FPC does not define UNICODE define because in Delphi it means more than just a
  // default string type but a full RTL implementation : using Windows 'W' functions instead of 'A',
  // another class declarations(TStringStream for example).
  {$IFDEF FPC_UNICODESTRINGS}
    {$DEFINE UNICODE}
  {$ENDIF}
{$ENDIF}


// Delphi compiler specific settings
//----------------------------------------------------------------------------------------------------
{$IFDEF WDC}
  // Project can be compiled without madExcept features (if you don't have madExcept installed)
  {$DEFINE USE_MAD_EXCEPT}
{$ENDIF}


// Promote some warnings to errors for safety (for all our tools)
//----------------------------------------------------------------------------------------------------
{$IFDEF WDC}
  {$WARN HIDING_MEMBER ERROR}         // W1009 Redeclaration of '%s' hides a member in the base class
  {$WARN HIDDEN_VIRTUAL ERROR}        // W1010 Method '%s' hides virtual method of base type '%s'
  {$WARN GARBAGE ERROR}               // W1011 Text after final 'END.' - ignored by compiler
  {$WARN BOUNDS_ERROR ERROR}          // W1012 Constant expression violates subrange bounds (Delphi)
  {$WARN FOR_VARIABLE ERROR}          // W1019 For loop control variable must be simple local variable (Delphi)
  {$WARN CONSTRUCTING_ABSTRACT ERROR} // W1020 Constructing instance of '%s' containing abstract method '%s.%s' (Delphi)
  {$WARN COMPARISON_FALSE ERROR}      // W1021 Comparison always evaluates to False
  {$WARN COMPARISON_TRUE ERROR}       // W1022 Comparison always evaluates to True
  {$WARN DUPLICATE_CTOR_DTOR ERROR}   // W1029 Duplicate %s '%s' with identical parameters will be inacessible from C++ (Delphi)
  {$WARN NO_RETVAL ERROR}             // W1035 Return value of function '%s' might be undefined
  {$WARN USE_BEFORE_DEF ERROR}        // W1036 Variable '%s' might not have been initialized
  {$WARN FOR_LOOP_VAR_UNDEF ERROR}    // W1037 FOR-Loop variable '%s' may be undefined after loop
  {$WARN UNIT_NAME_MISMATCH ERROR}    // W1038 Unit identifier '%s' does not match file name (Delphi)
  {$WARN DUPLICATES_IGNORED ERROR}    // W1051 Duplicate symbol names in namespace. Using '%s.%s' found in %s. Ignoring duplicate in %s
{$ENDIF}

// Aggregate defines for platforms
//----------------------------------------------------------------------------------------------------
{$IFDEF MSWINDOWS} {$DEFINE DESKTOP} {$ENDIF}
{$IFDEF UNIX}      {$DEFINE DESKTOP} {$ENDIF}
{$IFDEF MACOS}{$IFNDEF IOS}{$DEFINE DESKTOP}{$ENDIF}{$ENDIF} // Note that MACOS also indicates iOS
{$IFDEF ANDROID}   {$DEFINE TABLET} {$ENDIF}
{$IFDEF IOS}       {$DEFINE TABLET} {$ENDIF}


