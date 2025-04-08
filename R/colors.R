#' 'Flexoki' Color Palettes
#'
#' Color palettes from Steph Ango's <https://stephango.com/flexoki>.
#'
#' @format list of [palette::palette] vectors
#'
#' @examples
#' plot(flexoki$dark)
#' plot(flexoki$light)
#'
#' @concept colors
#' @export
flexoki <- list(
  dark = palette::palette(c(
    red600 = '#AF3029',
    orange600 = '#BC5215',
    yellow600 = '#AD8301',
    green600 = '#66800B',
    cyan600 = '#24837B',
    blue600 = '#205EA6',
    purple600 = '#5E409D',
    magenta600 = '#A02F6F'
  )),
  light = palette::palette(c(
    red400     = '#D14D41',
    orange400  = '#DA702C',
    yellow400  = '#D0A215',
    green400   = '#879A39',
    cyan400    = '#3AA99F',
    blue400    = '#4385BE',
    purple400  = '#8B7EC8',
    magenta400 = '#CE5D97'
  )),
  base = palette::palette(c(
    paper    = '#FFFCF0',
    base50   = '#F2F0E5',
    base100  = '#E6E4D9',
    base150  = '#DAD8CE',
    base200  = '#CECDC3',
    base300  = '#B7B5AC',
    base400  = '#9F9D96',
    base500  = '#878580',
    base600  = '#6F6E69',
    base700  = '#575653',
    base800  = '#403E3C',
    base850  = '#343331',
    base900  = '#282726',
    base950  = '#1C1B1A',
    black    = '#100F0F'
  )),
  red = palette::palette(c(
    red50  = '#FFE1D5',
    red100 = '#FFCABB',
    red150 = '#FDB2A2',
    red200 = '#F89A8A',
    red300 = '#E8705F',
    red400 = '#D14D41',
    red500 = '#C03E35',
    red600 = '#AF3029',
    red700 = '#942822',
    red800 = '#6C201C',
    red850 = '#551B18',
    red900 = '#3E1715',
    red950 = '#261312'
  )),
  orange = palette::palette(c(
    orange50  = '#FFE7CE',
    orange100 = '#FED3AF',
    orange150 = '#FCC192',
    orange200 = '#F9AE77',
    orange300 = '#EC8B49',
    orange400 = '#DA702C',
    orange500 = '#CB6120',
    orange600 = '#BC5215',
    orange700 = '#9D4310',
    orange800 = '#71320D',
    orange850 = '#59290D',
    orange900 = '#40200D',
    orange950 = '#27180E'
  )),
  yellow = palette::palette(c(
    yellow50  = '#FAEEC6',
    yellow100 = '#F6E2A0',
    yellow150 = '#F1D67E',
    yellow200 = '#ECCB60',
    yellow300 = '#DFB431',
    yellow400 = '#D0A215',
    yellow500 = '#BE9207',
    yellow600 = '#AD8301',
    yellow700 = '#8E6B01',
    yellow800 = '#664D01',
    yellow850 = '#503D02',
    yellow900 = '#3A2D04',
    yellow950 = '#241E08'
  )),
  green = palette::palette(c(
    green50  = '#EDEECF',
    green100 = '#DDE2B2',
    green150 = '#CDD597',
    green200 = '#BEC97E',
    green300 = '#A0AF54',
    green400 = '#879A39',
    green500 = '#768D21',
    green600 = '#66800B',
    green700 = '#536907',
    green800 = '#3D4C07',
    green850 = '#313D07',
    green900 = '#252D09',
    green950 = '#1A1E0C'
  )),
  cyan = palette::palette(c(
    cyan50  = '#DDF1E4',
    cyan100 = '#BFE8D9',
    cyan150 = '#A2DECE',
    cyan200 = '#87D3C3',
    cyan300 = '#5ABDAC',
    cyan400 = '#3AA99F',
    cyan500 = '#2F968D',
    cyan600 = '#24837B',
    cyan700 = '#1C6C66',
    cyan800 = '#164F4A',
    cyan850 = '#143F3C',
    cyan900 = '#122F2C',
    cyan950 = '#101F1D'
  )),
  blue = palette::palette(c(
    blue50  = '#E1ECEB',
    blue100 = '#C6DDE8',
    blue150 = '#ABCFE2',
    blue200 = '#92BFDB',
    blue300 = '#66A0C8',
    blue400 = '#4385BE',
    blue500 = '#3171B2',
    blue600 = '#205EA6',
    blue700 = '#1A4F8C',
    blue800 = '#163B66',
    blue850 = '#133051',
    blue900 = '#12253B',
    blue950 = '#101A24'
  )),
  purple = palette::palette(c(
    purple50  = '#F0EAEC',
    purple100 = '#E2D9E9',
    purple150 = '#D3CAE6',
    purple200 = '#C4B9E0',
    purple300 = '#A699D0',
    purple400 = '#8B7EC8',
    purple500 = '#735EB5',
    purple600 = '#5E409D',
    purple700 = '#4F3685',
    purple800 = '#3C2A62',
    purple850 = '#31234E',
    purple900 = '#261C39',
    purple950 = '#1A1623'
  )),
  magenta = palette::palette(c(
    magenta50  = '#FEE4E5',
    magenta100 = '#FCCFDA',
    magenta150 = '#F9B9CF',
    magenta200 = '#F4A4C2',
    magenta300 = '#E47DA8',
    magenta400 = '#CE5D97',
    magenta500 = '#B74583',
    magenta600 = '#A02F6F',
    magenta700 = '#87285E',
    magenta800 = '#641F46',
    magenta850 = '#4F1B39',
    magenta900 = '#39172B',
    magenta950 = '#24131D'
  ))
)
