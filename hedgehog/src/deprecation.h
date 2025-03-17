#define DEPRECATE_DATE(version) 2025–04–30

#define DEPRECATE(ident, message, version) \
{-# DEPRECATED \
  ident \
  [ message, \
    "Supported for one year after #version or higher is released (>=#DEPRECATE_DATE(version))" \
  ] \
  #-}
