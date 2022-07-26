module static;

op ==(i64, i64) -> bool;
op !=(i64, i64) -> bool;
op >(i64, i64) -> bool;
op >=(i64, i64) -> bool;
op <(i64, i64) -> bool;
op <=(i64, i64) -> bool;
op &(i64, i64) -> i64;
op |(i64, i64) -> i64;
op ^(i64, i64) -> i64;
op ..(i64, i64) -> Range<i64>;
op ..=(i64, i64) -> RangeInclusive<i64>;

op ==(bool, bool) -> bool;
op !=(bool, bool) -> bool;
op >(bool, bool) -> bool;
op >=(bool, bool) -> bool;
op <(bool, bool) -> bool;
op <=(bool, bool) -> bool;
op &(bool, bool) -> bool;
op |(bool, bool) -> bool;
op ^(bool, bool) -> bool;

op ==((), ()) -> bool;
op !=((), ()) -> bool;
op >((), ()) -> bool;
op >=((), ()) -> bool;
op <((), ()) -> bool;
op <=((), ()) -> bool;

op +(i64, i64) -> i64;
op -(i64, i64) -> i64;
op *(i64, i64) -> i64;
op /(i64, i64) -> i64;
op %(i64, i64) -> i64;
op **(i64, i64) -> i64;
op >>(i64, i64) -> i64;
op <<(i64, i64) -> i64;

op +(f64, f64) -> f64;
op -(f64, f64) -> f64;
op *(f64, f64) -> f64;
op /(f64, f64) -> f64;
op %(f64, f64) -> f64;
op **(f64, f64) -> f64;
op ==(f64, f64) -> bool;
op !=(f64, f64) -> bool;
op >(f64, f64) -> bool;
op >=(f64, f64) -> bool;
op <(f64, f64) -> bool;
op <=(f64, f64) -> bool;

op +(f64, i64) -> f64;
op -(f64, i64) -> f64;
op *(f64, i64) -> f64;
op /(f64, i64) -> f64;
op %(f64, i64) -> f64;
op **(f64, i64) -> f64;
op ==(f64, i64) -> bool;
op !=(f64, i64) -> bool;
op >(f64, i64) -> bool;
op >=(f64, i64) -> bool;
op <(f64, i64) -> bool;
op <=(f64, i64) -> bool;

op +(i64, f64) -> f64;
op -(i64, f64) -> f64;
op *(i64, f64) -> f64;
op /(i64, f64) -> f64;
op %(i64, f64) -> f64;
op **(i64, f64) -> f64;
op ==(i64, f64) -> bool;
op !=(i64, f64) -> bool;
op >(i64, f64) -> bool;
op >=(i64, f64) -> bool;
op <(i64, f64) -> bool;
op <=(i64, f64) -> bool;

op +(Decimal, Decimal) -> Decimal;
op -(Decimal, Decimal) -> Decimal;
op *(Decimal, Decimal) -> Decimal;
op /(Decimal, Decimal) -> Decimal;
op %(Decimal, Decimal) -> Decimal;
op **(Decimal, Decimal) -> Decimal;
op ==(Decimal, Decimal) -> bool;
op !=(Decimal, Decimal) -> bool;
op >(Decimal, Decimal) -> bool;
op >=(Decimal, Decimal) -> bool;
op <(Decimal, Decimal) -> bool;
op <=(Decimal, Decimal) -> bool;

op +(Decimal, i64) -> Decimal;
op -(Decimal, i64) -> Decimal;
op *(Decimal, i64) -> Decimal;
op /(Decimal, i64) -> Decimal;
op %(Decimal, i64) -> Decimal;
op **(Decimal, i64) -> Decimal;
op ==(Decimal, i64) -> bool;
op !=(Decimal, i64) -> bool;
op >(Decimal, i64) -> bool;
op >=(Decimal, i64) -> bool;
op <(Decimal, i64) -> bool;
op <=(Decimal, i64) -> bool;

op +(i64, Decimal) -> Decimal;
op -(i64, Decimal) -> Decimal;
op *(i64, Decimal) -> Decimal;
op /(i64, Decimal) -> Decimal;
op %(i64, Decimal) -> Decimal;
op **(i64, Decimal) -> Decimal;
op ==(i64, Decimal) -> bool;
op !=(i64, Decimal) -> bool;
op >(i64, Decimal) -> bool;
op >=(i64, Decimal) -> bool;
op <(i64, Decimal) -> bool;
op <=(i64, Decimal) -> bool;

op +(String, String) -> String;
op -(String, String) -> String;
op ==(String, String) -> bool;
op !=(String, String) -> bool;
op >(String, String) -> bool;
op >=(String, String) -> bool;
op <(String, String) -> bool;
op <=(String, String) -> bool;

op +(char, char) -> String;
op ==(char, char) -> bool;
op !=(char, char) -> bool;
op >(char, char) -> bool;
op >=(char, char) -> bool;
op <(char, char) -> bool;
op <=(char, char) -> bool;

op +(char, String) -> String;
op ==(char, String) -> bool;
op !=(char, String) -> bool;
op >(char, String) -> bool;
op >=(char, String) -> bool;
op <(char, String) -> bool;
op <=(char, String) -> bool;

op +(String, char) -> String;
op -(String, char) -> String;
op ==(String, char) -> bool;
op !=(String, char) -> bool;
op >(String, char) -> bool;
op >=(String, char) -> bool;
op <(String, char) -> bool;
op <=(String, char) -> bool;

op +((), String) -> String;
op ==((), String) -> bool;
op !=((), String) -> bool;
op >((), String) -> bool;
op >=((), String) -> bool;
op <((), String) -> bool;
op <=((), String) -> bool;

op +(String, ()) -> String;
op ==(String, ()) -> bool;
op !=(String, ()) -> bool;
op >(String, ()) -> bool;
op >=(String, ()) -> bool;
op <(String, ()) -> bool;
op <=(String, ()) -> bool;

op +(Blob, Blob) -> Blob;
op +(Blob, char) -> Blob;
op ==(Blob, Blob) -> bool;
op !=(Blob, Blob) -> bool;


op ==(Range<i64>, RangeInclusive<i64>) -> bool;
op !=(Range<i64>, RangeInclusive<i64>) -> bool;

op ==(RangeInclusive<i64>, Range<i64>) -> bool;
op !=(RangeInclusive<i64>, Range<i64>) -> bool;

op ==(Range<i64>, Range<i64>) -> bool;
op !=(Range<i64>, Range<i64>) -> bool;

op ==(RangeInclusive<i64>, RangeInclusive<i64>) -> bool;
op !=(RangeInclusive<i64>, RangeInclusive<i64>) -> bool;

op ==(?, ?) -> bool;
op !=(?, ?) -> bool;
op >(?, ?) -> bool;
op >=(?, ?) -> bool;
op <(?, ?) -> bool;
op <=(?, ?) -> bool;


op &=(bool, bool);
op |=(bool, bool);

op +=(i64, i64);
op -=(i64, i64);
op *=(i64, i64);
op /=(i64, i64);
op %=(i64, i64);
op **=(i64, i64);
op >>=(i64, i64);
op <<=(i64, i64);
op &=(i64, i64);
op |=(i64, i64);
op ^=(i64, i64);

op +=(f64, f64);
op -=(f64, f64);
op *=(f64, f64);
op /=(f64, f64);
op %=(f64, f64);
op **=(f64, f64);

op +=(f64, i64);
op -=(f64, i64);
op *=(f64, i64);
op /=(f64, i64);
op %=(f64, i64);
op **=(f64, i64);

op +=(Decimal, Decimal);
op -=(Decimal, Decimal);
op *=(Decimal, Decimal);
op /=(Decimal, Decimal);
op %=(Decimal, Decimal);
op **=(Decimal, Decimal);

op +=(Decimal, i64);
op -=(Decimal, i64);
op *=(Decimal, i64);
op /=(Decimal, i64);
op %=(Decimal, i64);
op **=(Decimal, i64);

op +=(String, String);
op -=(String, String);
op +=(String, char);
op -=(String, char);
op +=(char, String);
op +=(char, char);

op +=(arraArray, item: Array);
op +=(arraArray, item: ?);

op +=(Blob, Blob);
op +=(Blob, value: i64);
op +=(Blob, char);
op +=(Blob, String);
