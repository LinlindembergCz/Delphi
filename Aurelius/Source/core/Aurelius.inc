// Do not change anything below this line
//------------------------------------------------------------------------------
{$IF CompilerVersion >= 21}
  {$DEFINE DELPHI2010_LVL}
{$IFEND}

{$IF CompilerVersion >= 22}
  {$DEFINE DELPHIXE_LVL}
{$IFEND}

{$IF CompilerVersion >= 23}
  {$DEFINE DELPHIXE2_LVL}
{$IFEND}

{$IF CompilerVersion >= 24}
  {$LEGACYIFEND ON}
  {$DEFINE DELPHIXE3_LVL}
{$IFEND}

{$IF CompilerVersion >= 25}
  {$DEFINE DELPHIXE4_LVL}
{$IFEND}

{$IFDEF DELPHIXE2_LVL}
  {.$DEFINE BINDINGS}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$SCOPEDENUMS ON}
