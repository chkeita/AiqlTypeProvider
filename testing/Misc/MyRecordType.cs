// Decompiled with JetBrains decompiler
// Type: ProviderImplementation.ProvidedTypes.MyRecordType
// Assembly: AiqlTypeProvider, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: 5D2D2FBC-9A0B-0837-A745-0383BC2F2D5D
// Assembly location: C:\dev\AiqlTypeProvider\AiqlTypeProvider\bin\Debug\netstandard2.0\AiqlTypeProvider.dll

using Microsoft.FSharp.Core;
using System;
using System.Collections;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace ProviderImplementation.ProvidedTypes
{
  [CompilationMapping(SourceConstructFlags.RecordType)]
  [Serializable]
  public sealed class MyRecordType : IEquatable<MyRecordType>, IStructuralEquatable, IComparable<MyRecordType>, IComparable, IStructuralComparable
  {
    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    internal string Dummy1@;
    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    internal int Dummy2@;

    [CompilationMapping(SourceConstructFlags.Field, 0)]
    public string Dummy1
    {
      get
      {
        return this.Dummy1@;
      }
    }

    [CompilationMapping(SourceConstructFlags.Field, 1)]
    public int Dummy2
    {
      get
      {
        return this.Dummy2@;
      }
    }

    public MyRecordType(string dummy1, int dummy2)
    {
      this.Dummy1@ = dummy1;
      this.Dummy2@ = dummy2;
    }

    [CompilerGenerated]
    public override string ToString()
    {
      return ExtraTopLevelOperators.PrintFormatToString<FSharpFunc<MyRecordType, string>>((PrintfFormat<FSharpFunc<MyRecordType, string>, Unit, string, string>) new PrintfFormat<FSharpFunc<MyRecordType, string>, Unit, string, string, MyRecordType>("%+A")).Invoke(this);
    }

    [CompilerGenerated]
    public virtual int CompareTo(MyRecordType obj)
    {
      if (this != null)
      {
        if (obj == null)
          return 1;
        LanguagePrimitives.get_GenericComparer();
        int num = string.CompareOrdinal(this.Dummy1@, obj.Dummy1@);
        if (num < 0 || num > 0)
          return num;
        LanguagePrimitives.get_GenericComparer();
        int dummy2_1 = this.Dummy2@;
        int dummy2_2 = obj.Dummy2@;
        if (dummy2_1 < dummy2_2)
          return -1;
        return dummy2_1 > dummy2_2 ? 1 : 0;
      }
      return obj != null ? -1 : 0;
    }

    [CompilerGenerated]
    public virtual int CompareTo(object obj)
    {
      return this.CompareTo((MyRecordType) obj);
    }

    [CompilerGenerated]
    public virtual int CompareTo(object obj, IComparer comp)
    {
      MyRecordType myRecordType = (MyRecordType) obj;
      if (this != null)
      {
        if ((MyRecordType) obj == null)
          return 1;
        int num = string.CompareOrdinal(this.Dummy1@, myRecordType.Dummy1@);
        if (num < 0 || num > 0)
          return num;
        int dummy2_1 = this.Dummy2@;
        int dummy2_2 = myRecordType.Dummy2@;
        if (dummy2_1 < dummy2_2)
          return -1;
        return dummy2_1 > dummy2_2 ? 1 : 0;
      }
      return (MyRecordType) obj != null ? -1 : 0;
    }

    [CompilerGenerated]
    public virtual int GetHashCode(IEqualityComparer comp)
    {
      if (this == null)
        return 0;
      int num1 = 0;
      int num2 = this.Dummy2@ + ((num1 << 6) + (num1 >> 2)) - 1640531527;
      int num3 = -1640531527;
      string dummy1 = this.Dummy1@;
      int num4 = (dummy1 == null ? 0 : dummy1.GetHashCode()) + ((num2 << 6) + (num2 >> 2));
      return num3 + num4;
    }

    [CompilerGenerated]
    public override sealed int GetHashCode()
    {
      return this.GetHashCode(LanguagePrimitives.get_GenericEqualityComparer());
    }

    [CompilerGenerated]
    public virtual bool Equals(object obj, IEqualityComparer comp)
    {
      if (this == null)
        return obj == null;
      MyRecordType myRecordType1 = obj as MyRecordType;
      if (myRecordType1 == null)
        return false;
      MyRecordType myRecordType2 = myRecordType1;
      if (!string.Equals(this.Dummy1@, myRecordType2.Dummy1@))
        return false;
      return this.Dummy2@ == myRecordType2.Dummy2@;
    }

    [CompilerGenerated]
    public virtual bool Equals(MyRecordType obj)
    {
      if (this == null)
        return obj == null;
      if (obj != null && string.Equals(this.Dummy1@, obj.Dummy1@))
        return this.Dummy2@ == obj.Dummy2@;
      return false;
    }

    [CompilerGenerated]
    public override sealed bool Equals(object obj)
    {
      MyRecordType myRecordType = obj as MyRecordType;
      if (myRecordType != null)
        return this.Equals(myRecordType);
      return false;
    }
  }
}
