module GalleryUtils exposing 
  (
    makeDate,
    int2Time
  )

import Time
import Date


int2Time : Int -> Time.Time
int2Time i = 
  (toFloat i) * 1000.0

makeDate : Time.Time -> String
makeDate udate =
  let
    d = Date.fromTime udate
    day = Date.day d
    suffix = 
      if day // 10 == 1 then
        "th"
      else
        case day % 10 of
          1 -> "st"
          2 -> "nd"
          3 -> "rd"
          _ -> "th"
    month = 
      case Date.month d of
          Date.Jan -> "January"
          Date.Feb -> "February"
          Date.Mar -> "March"
          Date.Apr -> "April"
          Date.May -> "May"
          Date.Jun -> "June"
          Date.Jul -> "July"
          Date.Aug -> "August"
          Date.Sep -> "September"
          Date.Oct -> "October"
          Date.Nov -> "November"
          Date.Dec -> "December"
  in
    month ++ " " ++ (toString day) ++ suffix ++ ", " ++ toString (Date.year d)

