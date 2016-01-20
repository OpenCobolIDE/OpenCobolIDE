"""
This module contains the list of possible encodings, taken from
the standard library documentation:
https://docs.python.org/3.4/library/codecs.html#standard-encodings

"""

#: Encodings map, map a codec name to a an alias/language pair.#:
ENCODINGS_MAP = {
    "ascii": ("US-ASCII", "English"),
    "big5": ("Big5", "Chinese traditional"),
    "big5hkscs": ("Big5-HKSCS", "Chinese traditional"),
    "cp037": ("IBM037", "English"),
    "cp424": ("IBM242", "Hebrew"),
    "cp437": ("IBM437", "English"),
    "cp500": ("IBM500", "Western"),
    "cp737": ("IBM737", "Greek"),
    "cp775": ("IBM775", "Baltic"),
    "cp850": ("IBM850", "Western"),
    "cp852": ("IBM852", "Central European"),
    "cp855": ("IBM855", "Cyrillic"),
    "cp856": ("IBM856", "Hebrew"),
    "cp857": ("IBM857", "Turkish"),
    "cp860": ("IBM860", "Portugese"),
    "cp861": ("IBM861", "Icelandic"),
    "cp862": ("IBM862", "Hebrew"),
    "cp863": ("IBM863", "Canadian"),
    "cp864": ("IBM864", "Arabic"),
    "cp865": ("IBM865", "Nordic"),
    "cp866": ("IBM866", "Russian"),
    "cp869": ("IBM869", "Greek"),
    "cp874": ("IBM874", "Thai"),
    "cp875": ("IBM875", "Greek"),
    "cp932": ("IBM932", "Japanese"),
    "cp949": ("IBM949", "Korean"),
    "cp950": ("IBM950", "Chinese traditional"),
    "cp1006": ("IBM1006", "Urdu"),
    "cp1026": ("IBM1026", "Turkish"),
    "cp1140": ("IBM1140", "Western"),
    "cp1250": ("windows-1250", "Central European"),
    "cp1251": ("windows-1251", "Cyrillic"),
    "cp1252": ("windows-1252", "Western"),
    "cp1253": ("windows-1253", "Greek"),
    "cp1254": ("windows-1254", "      Turkish"),
    "cp1255": ("windows-1255", "Hebrew"),
    "cp1256": ("windows-1256", "      Arabic"),
    "cp1257": ("windows-1257", "Baltic"),
    "cp1258": ("windows-1258", "Vietnamese"),
    "euc_jp": ("EUC-JP", "Japanese"),
    "euc_jis_2004": ("EUC-JIS-2004", "Japanese"),
    "euc_jisx0213": ("EUC-JISX0213", "Japanese"),
    "euc_kr": ("EUC-KR", "Korean"),
    "gb2312": ("GB2312", "Chinese simplified"),
    "gbk": ("GBK", "Chinese unified"),
    "gb18030": ("GB18030", "Chinese unified"),
    "hz": ("HZ", "Chinese simplified"),
    "iso2022_jp": ("ISO-2022-JP", "Japanese"),
    "iso2022_jp_1": ("ISO-2022-JP-1", "Japanese"),
    "iso2022_jp_2": ("ISO-2022-JP-2", "Japanese"),
    "iso2022_jp_2004": ("ISO-2022-JP-2004", "Japanese"),
    "iso2022_jp_3": ("ISO-2022-JP-3", "Japanese"),
    "iso2022_jp_ext": ("ISO-2022-JP-EXT", "Japanese"),
    "iso2022_kr": ("ISO-2022-KR", "Korean"),
    "latin_1": ("ISO-8859-1", "Western"),
    "iso8859_2": ("ISO-8859-2", "Central European"),
    "iso8859_3": ("ISO-8859-3", "South European"),
    "iso8859_4": ("ISO-8859-4", "Baltic"),
    "iso8859_5": ("ISO-8859-5", "Cyrillic"),
    "iso8859_6": ("ISO-8859-6", "Arabic"),
    "iso8859_7": ("ISO-8859-7", "Greek"),
    "iso8859_8": ("ISO-8859-8", "Hebrew"),
    "iso8859_9": ("ISO-8859-9", "Turkish"),
    "iso8859_10": ("ISO-8859-10", "Nordic"),
    "iso8859_13": ("ISO-8859-13", "Baltic"),
    "iso8859_14": ("ISO-8859-14", "Celtic"),
    "iso8859_15": ("ISO-8859-15", "Western"),
    "johab": ("Johab", "Korean"),
    "koi8_r": ("KOI8-R", "Russian"),
    "koi8_u": ("KOI8-U", "Ukrainian"),
    "mac_cyrillic": ("MacCyrillic", "Cyrillic"),
    "mac_greek": ("MacGreek", "Greek"),
    "mac_iceland": ("MacIceland", "Icelandic"),
    "mac_latin2": ("MacCentralEurope", "Central European"),
    "mac_roman": ("MacRoman", "Western"),
    "mac_turkish": ("MacTurkish", "Turkish"),
    "ptcp154": ("PTCP154", "Cyrillic Asian"),
    "shift_jis": ("Shift_JIS", "Japanese"),
    "shift_jis_2004": ("Shift_JIS-2004", "Japanese"),
    "shift_jisx0213": ("Shift_JISX0213", "Japanese"),
    "utf_16": ("UTF-16", "Unicode"),
    "utf_16_be": ("UTF-16BE", "Unicode"),
    "utf_16_le": ("UTF-16LE", "Unicode"),
    "utf_7": ("UTF-7", "Unicode"),
    "utf_8": ("UTF-8", "Unicode")
}


def convert_to_codec_key(value):
    """
    Normalize code key value (encoding codecs must be lower case and must
    not contain any dashes).

    :param value: value to convert.
    """
    if not value:
        # fallback to utf-8
        value = 'UTF-8'
    # UTF-8 -> utf_8
    converted = value.replace('-', '_').lower()
    # fix some corner cases, see https://github.com/pyQode/pyQode/issues/11
    all_aliases = {
        'ascii': [
            'us_ascii',
            'us',
            'ansi_x3.4_1968',
            'cp367',
            'csascii',
            'ibm367',
            'iso_ir_6',
            'iso646_us',
            'iso_646.irv:1991'
        ],
        'utf-7': [
            'csunicode11utf7',
            'unicode_1_1_utf_7',
            'unicode_2_0_utf_7',
            'x_unicode_1_1_utf_7',
            'x_unicode_2_0_utf_7',
        ],
        'utf_8': [
            'unicode_1_1_utf_8',
            'unicode_2_0_utf_8',
            'x_unicode_1_1_utf_8',
            'x_unicode_2_0_utf_8',
        ],
        'utf_16': [
            'utf_16le',
            'ucs_2',
            'unicode',
            'iso_10646_ucs2'
        ],
        'latin_1': ['iso_8859_1']
    }

    for key, aliases in all_aliases.items():
        if converted in aliases:
            return key
    return converted
