
<!-- README.md is generated from README.Rmd. Please edit that file -->

# railwayJP

## Overview

`railwayJP` is an R package for creating maps of railway routes across
Japan.

It uses the National Land Numerical Information (Railway Data) from the
Ministry of Land, Infrastructure, Transport and Tourism (MLIT).

## Installation

``` r
# Install from GitHub
# devtools::install_github("soichiro/railwayJP")
```

## Quick Start

### 1. Download the data

The railway data is not included in the package due to size (~20MB).
Download it on first use:

``` r
download_railway_data()
#> Railway data already exists at: ~/Library/Caches/railwayJP/UTF-8
#> Use force = TRUE to re-download
```

### 2. Plot a simple route

``` r
plot_railway_segments(
  segments = list(
    list(start = "松山", end = "高松", operator = "四国旅客鉄道", label = "予讃線")
  ),
  key_stations = c(
    "高松", "坂出", "宇多津", "丸亀", "多度津", "観音寺", "川之江", "伊予三島", "新居浜",
    "伊予西条", "壬生川", "今治", "伊予北条", "松山"
  ),
  output_file = "man/figures/matsuyama_takamatsu.png"
)
#> Loading railway data...
#> Data loaded: 21916 railroad sections, 10220 stations
#> 
#> Processing segment 1 : All lines 松山 -> 高松
#>   Using 559 sections, 271 station records
#>   Building track graph...
#>     Nodes: 541 Sections: 559
#>   Waypoints: 松山 -> 高松
#>   Finding path from 松山 to 高松 ...
#>     Found 117 sections
#>   Total path sections: 117
#>   Route: 松山 -> 三津浜 -> 伊予和気 -> 堀江 -> 光洋台 -> 粟井 -> 柳原 -> 伊予北条 -> 大浦 -> 浅海 -> 菊間 -> 伊予亀岡 -> 大西 -> 伊予三芳 -> 伊予富田 -> 伊予桜井 -> 壬生川 -> 玉之江 -> 波方 -> 今治 -> 伊予小松 -> 波止浜 -> 伊予氷見 -> 石鎚山 -> 伊予西条 -> 中萩 -> 新居浜 -> 多喜浜 -> 関川 -> 伊予土居 -> 赤星 -> 伊予寒川 -> 伊予三島 -> 川之江 -> 箕浦 -> 豊浜 -> 観音寺 -> 本山 -> 比地大 -> 高瀬 -> 詫間 -> みの -> 津島ノ宮 -> 海岸寺 -> 多度津 -> 讃岐塩屋 -> 丸亀 -> 宇多津 -> 坂出 -> 八十場 -> 鴨川 -> 讃岐府中 -> 国分 -> 端岡 -> 鬼無 -> 香西 -> 昭和町 -> 高松
#>   Result: 117 sections, 58 stations
#> 
#> Map saved to man/figures/matsuyama_takamatsu.png
```

<figure>
<img src="man/figures/matsuyama_takamatsu.png"
alt="Matsuyama to Takamatsu route" />
<figcaption aria-hidden="true">Matsuyama to Takamatsu route</figcaption>
</figure>

- `key_stations`: An optional parameter that allows highlighting
  specific stations with labels along the route.

#### Tips: Specifying operator and line names

It is important to provide accurate operator and line names to avoid
ambiguity when the origin or destination stations are served by multiple
lines.

``` r
routes <- get_route(
  list(
    # Does not specify operator - return routes that include multiple lines
    list(start = "松山", end = "三津浜"),
    # Specify operator to get a unique route
    list(start = "松山", end = "三津浜", operator = "四国旅客鉄道")
  )
)
#> 
#> [1] 松山 -> 三津浜
#>   Filter: All lines
#>   Available: 21916 sections, 10220 stations
#>   Building track graph...
#>     Nodes: 20608 Sections: 21916
#>   Waypoints: 松山 -> 三津浜
#>   Finding path from 松山 to 三津浜 ...
#>     Found 1 sections
#>   Total path sections: 1
#>   Route: 松山 -> ＪＲ松山駅前 -> 宮田町 -> 衣山 -> 西衣山 -> 三津浜
#>   Result: 1 sections, 6 stations
#> 
#> [2] 松山 -> 三津浜
#>   Filter: All lines (四国旅客鉄道)
#>   Available: 559 sections, 271 stations
#>   Building track graph...
#>     Nodes: 541 Sections: 559
#>   Waypoints: 松山 -> 三津浜
#>   Finding path from 松山 to 三津浜 ...
#>     Found 1 sections
#>   Total path sections: 1
#>   Route: 松山 -> 三津浜
#>   Result: 1 sections, 2 stations
```

- In the first example, without specifying an operator, the algorithm
  finds a route that includes segments from 伊予鉄道 (Iyo Railway).
  However, there is no direct connection between 西衣山 and 三津浜 on
  this route.
- In the second example, by specifying `operator = "四国旅客鉄道"` (JR
  Shikoku), the algorithm correctly identifies the 予讃線 (Yosan Line)
  route.

### 3. Plot multiple routes with via points

The function supports a segment that spans over multiple lines. For
example, going from 新宿 to 長野 using 在来線, we need to transfer at
松本.

``` r
plot_railway_segments(
  segments = list(
    # Route via Matsumoto (Chuo Line) instead of Shinkansen
    list(
      start = "新宿",
      end = "長野",
      via = c("松本"),
      operator = "東日本旅客鉄道",
      label = "中央線・篠ノ井線"
    )
  ),
  title = "Railway Routes",
  key_stations = c("新宿", "松本", "長野"),
  show_legend = TRUE,
  output_file = "man/figures/railway_routes.png"
)
#> 
#> Processing segment 1 : All lines 新宿 -> 長野
#>   Using 4146 sections, 1802 station records
#>   Building track graph...
#>     Nodes: 3818 Sections: 4146
#>   Waypoints: 新宿 -> 松本 -> 長野
#>   Finding path from 新宿 to 松本 ...
#>     Found 166 sections
#>   Finding path from 松本 to 長野 ...
#>     Found 26 sections
#>   Total path sections: 192
#>   Route: 新宿 -> 大久保 -> 新大久保 -> 東中野 -> 中野 -> 高円寺 -> 阿佐ヶ谷 -> 荻窪 -> 西荻窪 -> 吉祥寺 -> 三鷹 -> 武蔵境 -> 東小金井 -> 武蔵小金井 -> 国分寺 -> 西国分寺 -> 国立 -> 立川 -> 日野 -> 豊田 -> 八王子 -> 西八王子 -> 高尾 -> 相模湖 -> 藤野 -> 上野原 -> 四方津 -> 梁川 -> 鳥沢 -> 猿橋 -> 大月 -> 初狩 -> 笹子 -> 甲斐大和 -> 勝沼ぶどう郷 -> 塩山 -> 東山梨 -> 山梨市 -> 春日居町 -> 石和温泉 -> 酒折 -> 甲府 -> 竜王 -> 塩崎 -> 韮崎 -> 新府 -> 穴山 -> 日野春 -> 長坂 -> 小淵沢 -> 信濃境 -> 富士見 -> すずらんの里 -> 青柳 -> 茅野 -> 上諏訪 -> 下諏訪 -> 岡谷 -> みどり湖 -> 塩尻 -> 広丘 -> 村井 -> 平田 -> 南松本 -> 松本 -> 北松本 -> 冠着 -> 姨捨 -> 西条 -> 聖高原 -> 篠ノ井 -> 長野
#>   Result: 192 sections, 72 stations
#> 
#> Map saved to man/figures/railway_routes.png
```

<figure>
<img src="man/figures/railway_routes.png"
alt="Multiple railway routes" />
<figcaption aria-hidden="true">Multiple railway routes</figcaption>
</figure>

### 4. Multiple independent routes

You can plot multiple independent routes on the same map by providing
multiple segments.

``` r
plot_railway_segments(
  segments = list(
    # Route 1: Matsuyama to Shin-Kobe
    list(
      start = "松山",
      end = "岡山",
      via = "児島",
      label = "予讃線・本四備讃線",
      operator = c("四国旅客鉄道", "西日本旅客鉄道")
    ),
    list(
      start = "岡山",
      end = "新神戸",
      line = "山陽新幹線",
      label = "山陽新幹線"
    ),
    # Route 2: Nagoya to Toyama via Takayama Line
    list(
      start = "名古屋",
      end = "富山",
      via = c("岐阜", "高山"),
      operator = c("東海旅客鉄道", "西日本旅客鉄道"),
      label = "高山本線"
    )
  ),
  title = "Multiple Independent Routes",
  key_stations = c("松山", "岡山", "新神戸", "名古屋", "高山", "富山"),
  show_legend = TRUE,
  output_file = "man/figures/multiple_routes.png"
)
#> 
#> Processing segment 1 : All lines 松山 -> 岡山
#>   Using 3404 sections, 1557 station records
#>   Building track graph...
#>     Nodes: 3188 Sections: 3404
#>   Waypoints: 松山 -> 児島 -> 岡山
#>   Finding path from 松山 to 児島 ...
#>     Found 98 sections
#>   Finding path from 児島 to 岡山 ...
#>     Found 22 sections
#>   Total path sections: 120
#>   Route: 松山 -> 三津浜 -> 伊予和気 -> 堀江 -> 光洋台 -> 粟井 -> 柳原 -> 伊予北条 -> 大浦 -> 浅海 -> 菊間 -> 伊予亀岡 -> 大西 -> 伊予三芳 -> 伊予富田 -> 伊予桜井 -> 壬生川 -> 玉之江 -> 波方 -> 今治 -> 伊予小松 -> 波止浜 -> 伊予氷見 -> 石鎚山 -> 伊予西条 -> 中萩 -> 新居浜 -> 多喜浜 -> 関川 -> 伊予土居 -> 赤星 -> 伊予寒川 -> 伊予三島 -> 川之江 -> 箕浦 -> 豊浜 -> 観音寺 -> 本山 -> 比地大 -> 高瀬 -> 詫間 -> みの -> 津島ノ宮 -> 海岸寺 -> 多度津 -> 讃岐塩屋 -> 丸亀 -> 宇多津 -> 児島 -> 上の町 -> 木見 -> 植松 -> 茶屋町 -> 久々原 -> 早島 -> 備中箕島 -> 妹尾 -> 備前西市 -> 大元 -> 岡山
#>   Result: 120 sections, 60 stations
#> 
#> Processing segment 2 : 山陽新幹線 岡山 -> 新神戸
#>   Using 46 sections, 20 station records
#>   Building track graph...
#>     Nodes: 46 Sections: 46
#>   Waypoints: 岡山 -> 新神戸
#>   Finding path from 岡山 to 新神戸 ...
#>     Found 8 sections
#>   Total path sections: 8
#>   Route: 岡山 -> 相生 -> 姫路 -> 西明石 -> 新神戸
#>   Result: 8 sections, 5 stations
#> 
#> Processing segment 3 : All lines 名古屋 -> 富山
#>   Using 3876 sections, 1725 station records
#>   Building track graph...
#>     Nodes: 3606 Sections: 3876
#>   Waypoints: 名古屋 -> 岐阜 -> 高山 -> 富山
#>   Finding path from 名古屋 to 岐阜 ...
#>     Found 15 sections
#>   Finding path from 岐阜 to 高山 ...
#>     Found 54 sections
#>   Finding path from 高山 to 富山 ...
#>     Found 39 sections
#>   Total path sections: 107
#>   Route: 名古屋 -> 枇杷島 -> 清洲 -> 稲沢 -> 尾張一宮 -> 木曽川 -> 蘇原 -> 那加 -> 鵜沼 -> 各務ケ原 -> 長森 -> 岐阜 -> 坂祝 -> 美濃太田 -> 古井 -> 中川辺 -> 下麻生 -> 上麻生 -> 白川口 -> 下油井 -> 飛騨金山 -> 焼石 -> 下呂 -> 禅昌寺 -> 飛騨萩原 -> 上呂 -> 飛騨宮田 -> 飛騨小坂 -> 渚 -> 久々野 -> 飛騨一ノ宮 -> 高山 -> 上枝 -> 飛騨国府 -> 飛騨古川 -> 杉崎 -> 飛騨細江 -> 角川 -> 坂上 -> 打保 -> 杉原 -> 猪谷 -> 楡原 -> 笹津 -> 越中八尾 -> 東八尾 -> 千里 -> 速星 -> 婦中鵜坂 -> 西富山 -> 富山
#>   Result: 107 sections, 51 stations
#> 
#> Map saved to man/figures/multiple_routes.png
```

<figure>
<img src="man/figures/multiple_routes.png"
alt="Multiple independent routes" />
<figcaption aria-hidden="true">Multiple independent routes</figcaption>
</figure>

## Segment Options

Each segment in the `segments` list can have the following options:

| Option | Required | Description |
|----|----|----|
| `start` | Yes | Start station name (use `search_stations()` to find names) |
| `end` | Yes | End station name |
| `via` | No | Character vector of intermediate stations |
| `line` | No | Filter to specific line name, e.g., `"東海道線"` or `"東海道新幹線"` (use `search_lines()` to find names) |
| `operator` | No | Filter to specific operator (use `list_operators()` to see all) |
| `color` | No | Custom color (hex code) |
| `label` | No | Label for legend |

Use `search_lines()` to find the exact line name:

``` r
search_lines("東海道")
#>           line       operator n_sections n_stations
#> 1 東海道新幹線   東海旅客鉄道         44         17
#> 2     東海道線 東日本旅客鉄道        125         37
#> 3     東海道線   東海旅客鉄道        231         86
#> 4     東海道線 西日本旅客鉄道        130         52
```

## Plot Options

| Option              | Default           | Description                       |
|---------------------|-------------------|-----------------------------------|
| `output_file`       | “railway_map.png” | Output filename                   |
| `title`             | “Railway Routes”  | Plot title                        |
| `subtitle`          | (auto)            | Plot subtitle                     |
| `show_all_stations` | TRUE              | Show all stations along routes    |
| `key_stations`      | NULL              | Stations to highlight with labels |
| `show_legend`       | TRUE              | Show route legend                 |
| `width`             | 14                | Plot width (inches)               |
| `height`            | 10                | Plot height (inches)              |
| `dpi`               | 150               | Output resolution                 |

## Utility Functions

``` r
# List all railway lines
list_lines()

# Search for lines by name
list_lines("中央")

# List stations on a line
list_stations("中央線", operator = "東日本旅客鉄道")

# Search for stations
search_stations("新宿")

# List all operators
list_operators()

# Get data info
get_data_info()
```

## Operator Colors

The package includes corporate colors for major Japanese railway
operators:

``` r
# View all operator colors
list_operator_colors()

# Get color for specific operator
get_operator_color("東日本旅客鉄道")
```

``` r
# JR East green
railwayJP::get_operator_color("東日本旅客鉄道")
#> [1] "#008C4A"
```

## Data Source

This package uses the National Land Numerical Information (Railway Data)
from MLIT:

- **URL**: <https://nlftp.mlit.go.jp/ksj/>
- **Data version**: N02-22 (2022)
- **License**: MLIT Open Data License (allows redistribution and
  commercial use)
- **Attribution required**: “Source: National Land Numerical Information
  (Railway Data), MLIT”

## License

MIT License

The railway data is provided under the MLIT Open Data License, which
permits reproduction, redistribution, commercial use, and derivative
works with attribution.
