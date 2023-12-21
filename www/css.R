css <- HTML("

#fullAvgSocial > img, #fullAvgEcon > img, #fullAvgEnvi > img {
  position: relative;
  z-index:-1;
}

div > div.row.overlay2 > div > div:nth-child(3) > div:nth-child(3) {
  display:none;
}

.title-text {
  color: grey;
  font-size:12px;
}

#growthChoice {
  display:none;
}

.column {
  float: left;
  width: 100px;
}

.row:after {
  content: '';
  display: table;
  clear: both;
}

header {
	text-align: center;
	width: 100%;
	height: auto;
	background-size: cover;
	background-attachment: fixed;
	position: relative;
	overflow: hidden;
}

.overlay{
	width: 100%;
	height: 100%;
	padding: 50px;
	color: #FFF;
  background-image: linear-gradient( 135deg, #22a22069 10%, #22a22069 100%);
  border-radius: 0 0 85% 85% / 30%;
	
}

.overlay2{
	width: 100%;
	height: 100%;
	padding: 50px;
	color: #FFF;
  background-image: linear-gradient( 135deg, #22a22069 10%, #22a22069 100%);
  border-radius: 0 0 85% 85% / 30%;
	
}


#gaugeAVGecon > div.col-sm-4 {
  padding: 0px;
}

#gaugeAVGecon > div.col-sm-6 {
  padding: 0px;
}

#gaugeAVGecon > div.col-sm-2 {
  padding: 0px;
}

#gaugeAVGenvi > div.col-sm-4 {
  padding: 0px;
}

#gaugeAVGenvi > div.col-sm-6 {
  padding: 0px;
}

#gaugeAVGenvi > div.col-sm-2 {
  padding: 0px;
}

#gaugeAVGsocial > div.col-sm-4 {
  padding: 0px;
}

#gaugeAVGsocial > div.col-sm-6 {
  padding: 0px;
}

#gaugeAVGsocial > div.col-sm-2 {
  padding: 0px;
}

#gaugeAVGinsuff > div.col-sm-4 {
  padding: 0px;
}

#gaugeAVGinsuff > div.col-sm-6 {
  padding: 0px;
}

#gaugeAVGinsuff > div.col-sm-2 {
  padding: 0px;
}

h3 {
  margin-top:0px;
  margin-bottom:0px;
}

#gaugePlotAVG svg {
  align: right;
  border-color: black;
  color: black;
  height: 350px;
  border-style: none none solid solid;
}

#fullAvgEcon > svg > text:nth-child(6) > tspan, #fullAvgEnvi > svg > text:nth-child(6) > tspan, #fullAvgSocial > svg > text:nth-child(6) > tspan, #fullAvgInsuff > svg > text:nth-child(6) > tspan {
  font-weight: bold;
  font-size: 8px;
}

#fullAvgEcon {
  height:200px !important;
}

#fullAvgSocial, #fullAvgEnvi {
  height:200px !important;
}

#gaugeGoalAverage {
  height:225px !important;
}

#atgGauge, #dmaGauge, #knaGauge, #grdGauge, #lcaGauge, #vctGauge, #msrGauge {
  height: 120px !important;
}

#fullAvgEcon svg, #fullAvgEnvi svg, #fullAvgSocial svg {
  height: 225px !important;
}

#gaugeGoalAverage svg {
  height: 250px;
}

#dmaGauge > svg, #atgGauge > svg, #aiaGauge > svg, #grdGauge svg {
  height: 100% !important;
}

#msrGauge > svg, #vctGauge > svg, #lcaGauge > svg, #knaGauge svg {
  height: 100% !important;
}


h4 {
  align: center;
}

#gaugePlot2AVG svg {
  align: center;
  padding-top: 150px;
  padding-right: 250px;
  height: 500px;
  width: 900px;
}

#tableInsuff {
  background-color: #f5e2e2;
}

#tableInsuff thead tr:nth-child(1) {
  background-color: white;
}

#tableInsuff tbody tr:nth-child(1), #tableInsuff tr:nth-child(3), #tableInsuff tr:nth-child(8), #tableInsuff tr:nth-child(10), #tableInsuff tr:nth-child(13) {
background-color: whitesmoke;
}

#tableSocial tbody tr:nth-child(1){
  background-color: whitesmoke;
}
#tableSocial tr:nth-child(3), #tableSocial tr:nth-child(6), #tableSocial tr:nth-child(13) {
background-color: whitesmoke;
}


")

styler <- formatter("span", style = x ~ style(width="150px"))

improvement_formatter <- formatter("span",
                                   style = x ~ style(
                                     display = "inline-block", 
                                     color = "black",
                                     `background-color` = ifelse(as.numeric(str_replace(x, ",", "")) < 25, "#ff6f69", ifelse(between(as.numeric(str_replace(x, ",", "")), 25, 75),"#ffcc5c","#88d8b0")),
                                     display = "block", 
                                     padding = "0 4px",
                                     `border-radius` = "4px",
                                     width="75px"))

label_formatter <- formatter("span", style = x ~ style(width="75px"))

color_formatter <- formatter("span",
                             style = function(x) style(
                               display = "inline-block", 
                               direction = "rtl",
                               `unicode-bidi` = "plaintext",
                               `border-radius` = "4px",
                               `padding-left` = "2px",
                               `padding-right` = "2px", 
                               `background-color` = ifelse(between(as.numeric(str_replace(x, ",", "")), 0, 25), "#b220206e", ifelse(between(x, 25,75), "#b299206e", "#20b23f6e")), 
                               width = percent((x/100))))

                                     
unit.scale = function(x) (x - 0) / (100 - 0)


