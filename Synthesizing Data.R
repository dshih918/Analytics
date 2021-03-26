#The code is breifly divided into 3 parts.
# 1- Recategorization of variables Sport and Column
# 2- Generating csv files for Mapped IDs  
# 3- Generating Synthetic data 


rm(list=ls())

library(data.table)
library(readxl)
library(plyr)
library(synthpop)

#Load the dataset
#Change the folder path 
myData <- read_excel("C:\\Users\\DD\\Documents\\DANA 4850\\Test_Synth.xlsx")

str(myData)
myData<-as.data.frame(myData)
str(myData)
colnames(myData)
colnames(myData) <- c("Test.Type", "Org.ID", "Team.ID", "Sport", "Athlete.ID", "Birthdate", "Gender", "Height", "Position", "Examiner.ID",
                      "Examiner.Role", "Assessment.Date", "Injury.Date", "Concussion.Diagnosed.Decision", "Removed.from.Play", 
                      "Assessment.ID","Fatigue", "Activity.Type", "Test.collection", "RTP.ID") 
#View(myData)

#---------------------x-----------------------------1--------------------------------x-------------------------
i=0
while (i < 1) {
  myData$Sport[myData$Sport == "baseball"] <- "Baseball"
  myData$Sport[myData$Sport == "Girls Basketball"] <- "Basketball"
  myData$Sport[myData$Sport == "Cheer"] <- "Cheerleading"
  myData$Sport[myData$Sport == "curling"] <- "Curling"
  myData$Sport[myData$Sport == "Fencing"] <- "Other"
  myData$Sport[myData$Sport == "football"] <- "Football"
  myData$Sport[myData$Sport == "frisbee"] <- "Frisbee"
  myData$Sport[myData$Sport == "ultimate"] <- "Frisbee"
  myData$Sport[myData$Sport == "Ultimate Frisbee"] <- "Frisbee"
  myData$Sport[myData$Sport == "hockey"] <- "Hockey"
  myData$Sport[myData$Sport == "Ice Hockey"] <- "Hockey"
  myData$Sport[myData$Sport == "Horse racing"] <- "Horse Racing"
  myData$Sport[myData$Sport == "Box Lacrosse"] <- "Lacrosse"
  myData$Sport[myData$Sport == "Field Lacrosse"] <- "Lacrosse"
  myData$Sport[myData$Sport == "lacrosse"] <- "Lacrosse"
  myData$Sport[myData$Sport == "lax"] <- "Lacrosse"
  myData$Sport[myData$Sport == " "] <- "Other"
  myData$Sport[is.na(myData$Sport)] <- "Other"
  myData$Sport[myData$Sport == "4X"] <- "Other"
  myData$Sport[myData$Sport == "All population"] <- "Other"
  myData$Sport[myData$Sport == "Aussie Rules Football"] <- "Other"
  myData$Sport[myData$Sport == "Ax Throwing"] <- "Other"
  myData$Sport[myData$Sport == "Baseline Testing"] <- "Other"
  myData$Sport[myData$Sport == "Certification"] <- "Other"
  myData$Sport[myData$Sport == "fdsfs"] <- "Other"
  myData$Sport[myData$Sport == "General student body"] <- "Other"
  myData$Sport[myData$Sport == "horseback riding"] <- "Other"
  myData$Sport[myData$Sport == "horesback riding"] <- "Other"
  myData$Sport[myData$Sport == "hurling"] <- "Other"
  myData$Sport[myData$Sport == "karate"] <- "Other"
  myData$Sport[myData$Sport == "medicine"] <- "Other"
  myData$Sport[myData$Sport == "Mochi Ball"] <- "Other"
  myData$Sport[myData$Sport == "N/A"] <- "Other"
  myData$Sport[myData$Sport == "Pearling"] <- "Other"
  myData$Sport[myData$Sport == "Quidditch"] <- "Other"
  myData$Sport[myData$Sport == "Ringette"] <- "Other"
  myData$Sport[myData$Sport == "Roller Hockey"] <- "Other"
  myData$Sport[myData$Sport == "Scouts"] <- "Other"
  myData$Sport[myData$Sport == "Snowboard"] <- "Other"
  myData$Sport[myData$Sport == "Speed Skating"] <- "Other"
  myData$Sport[myData$Sport == "Sports Med"] <- "Other"
  myData$Sport[myData$Sport == "Squash"] <- "Other"
  myData$Sport[myData$Sport == "Swimming"] <- "Other"
  myData$Sport[myData$Sport == "Taekwondo"] <- "Other"
  myData$Sport[myData$Sport == "tennis"] <- "Other"
  myData$Sport[myData$Sport == "Training"] <- "Other"
  myData$Sport[myData$Sport == "Varied"] <- "Other"
  myData$Sport[myData$Sport == "BC Rugby"] <- "Rugby"
  myData$Sport[myData$Sport == "Rugby 15's"] <- "Rugby"
  myData$Sport[myData$Sport == "Rugby 7's"] <- "Rugby"
  myData$Sport[myData$Sport == "Cross Country"] <- "Skiing"
  myData$Sport[myData$Sport == "Down Hill Skiing"] <- "Skiing"
  myData$Sport[myData$Sport == "Indoor Soccer"] <- "Soccer"
  myData$Sport[myData$Sport == "Outdoor Soccer"] <- "Soccer"
  myData$Sport[myData$Sport == "soccer"] <- "Soccer"
  myData$Sport[myData$Sport == "vball"] <- "Volleyball"
  myData$Sport[myData$Sport == "vollyball"] <- "Volleyball"
  i=1
}


j=0
while (j < 1) {
  myData$Position[myData$Position == " "] <- "N/A"
  myData$Position[myData$Position == "-"] <- "N/A"
  myData$Position[is.na(myData$Position)] <- "N/A"
  myData$Position[myData$Position == "center"] <- "Forward"
  myData$Position[myData$Position == "/D"] <- "Defender"
  myData$Position[myData$Position == "0"] <- "Other"
  myData$Position[myData$Position == "10"] <- "Back"
  myData$Position[myData$Position == "12"] <- "Back"
  myData$Position[myData$Position == "13"] <- "Back"
  myData$Position[myData$Position == "1st & 3rd"] <- "Infield"
  myData$Position[myData$Position == "1st base"] <- "Infield"
  myData$Position[myData$Position == "2nd"] <- "Infield"
  myData$Position[myData$Position == "2nd Base"] <- "Infield"
  myData$Position[myData$Position == "2nd Row"] <- "Forward"
  myData$Position[myData$Position == "2ndbase"] <- "Infield"
  myData$Position[myData$Position == "8"] <- "Forward"
  myData$Position[myData$Position == "8 man, flanker"] <- "Forward"
  myData$Position[myData$Position == "A"] <- "Other"
  myData$Position[myData$Position == "ailier"] <- "Forward"
  myData$Position[myData$Position == "Ailier"] <- "Forward"
  myData$Position[myData$Position == "ailier gauche"] <- "Forward"
  myData$Position[myData$Position == "Ailier Gauche"] <- "Forward"
  myData$Position[myData$Position == "Ailier/centr"] <- "Forward"
  myData$Position[myData$Position == "Aillier Gauche"] <- "Forward"
  myData$Position[myData$Position == "attack"] <- "Forward"
  myData$Position[myData$Position == "Attack"] <- "Forward"
  myData$Position[myData$Position == "Attack/midfield"] <- "Multiple"
  myData$Position[myData$Position == "attaquant"] <- "Forward"
  myData$Position[myData$Position == "Attaquant"] <- "Forward"
  myData$Position[myData$Position == "Back"] <- "Back"
  myData$Position[myData$Position == "Back 3"] <- "Back"
  myData$Position[myData$Position == "back row A"] <- "Back"
  myData$Position[myData$Position == "Bike"] <- "Other"
  myData$Position[myData$Position == "both"] <- "Multiple"
  myData$Position[myData$Position == "C"] <- "Forward"
  myData$Position[myData$Position == "C or RW"] <- "Forward"
  myData$Position[myData$Position == "C R"] <- "Forward"
  myData$Position[myData$Position == "c/ le"] <- "Forward"
  myData$Position[myData$Position == "c/ pf"] <- "Forward"
  myData$Position[myData$Position == "C/LW"] <- "Forward"
  myData$Position[myData$Position == "C/Rw"] <- "Forward"
  myData$Position[myData$Position == "C/RW"] <- "Forward"
  myData$Position[myData$Position == "C/UT"] <- "Forward"
  myData$Position[myData$Position == "CAM/LW"] <- "Multiple"
  myData$Position[myData$Position == "catcher"] <- "Multiple"
  myData$Position[myData$Position == "Catcher"] <- "Forward"
  myData$Position[myData$Position == "cdm/cm"] <- "Forward"
  myData$Position[myData$Position == "cdm/cm/cam"] <- "Forward"
  myData$Position[myData$Position == "centee"] <- "Forward"
  myData$Position[myData$Position == "center"] <- "Forward"
  myData$Position[myData$Position == "Center"] <- "Forward"
  myData$Position[myData$Position == "center"] <- "Forward"
  myData$Position[myData$Position == "Center -shoots Right"] <- "Forward"
  myData$Position[myData$Position == "Center -shoots Left"] <- "Forward"
  myData$Position[myData$Position == "Center back"] <- "Defender"
  myData$Position[myData$Position == "Center field"] <- "Outfield"
  myData$Position[myData$Position == "Center or Left Wing"] <- "Forward"
  myData$Position[myData$Position == "Center/ Left wing"] <- "Forward"
  myData$Position[myData$Position == "center/ winger"] <- "Forward"
  myData$Position[myData$Position == "center/wing"] <- "Forward"
  myData$Position[myData$Position == "center\\ wing"] <- "Forward"
  myData$Position[myData$Position == "Center/wing"] <- "Forward"
  myData$Position[myData$Position == "center\ wing"] <- "Forward"
  myData$Position[myData$Position == "centre"] <- "Forward"
  myData$Position[myData$Position == "Centre"] <- "Forward"
  myData$Position[myData$Position == "Centre-Left Wing  shoots Left"] <- "Forward"
  myData$Position[myData$Position == "Centre-shoots Right"] <- "Forward"
  myData$Position[myData$Position == "Center  -shoots Left"] <- "Forward"
  myData$Position[myData$Position == "Centre - shoots Left"] <- "Forward"
  myData$Position[myData$Position == "centre -shoots L"] <- "Forward"
  myData$Position[myData$Position == "Centre -shoots L"] <- "Forward"
  myData$Position[myData$Position == "Centre -shoots Right"] <- "Forward"
  myData$Position[myData$Position == "centre mid"] <- "Forward"
  myData$Position[myData$Position == "Centre shoots Left"] <- "Forward"
  myData$Position[myData$Position == "Centre/right wing"] <- "Forward"
  myData$Position[myData$Position == "Centrr"] <- "Forward"
  myData$Position[myData$Position == "Cf"] <- "Outfield"
  myData$Position[myData$Position == "Cornerback"] <- "Defender"
  myData$Position[myData$Position == "CW"] <- "Forward"
  myData$Position[myData$Position == "d"] <- "Defender"
  myData$Position[myData$Position == "D"] <- "Defender"
  myData$Position[myData$Position == "D/T"] <- "Defender"
  myData$Position[myData$Position == "Défenseur"] <- "Defender"
  myData$Position[myData$Position == "défenseur"] <- "Defender"
  myData$Position[myData$Position == "db"] <- "Defender"
  myData$Position[myData$Position == "Db"] <- "Defender"
  myData$Position[myData$Position == "DB"] <- "Defender"
  myData$Position[myData$Position == "DE"] <- "Defender"
  myData$Position[myData$Position == "DE/Tackle"] <- "Defender"
  myData$Position[myData$Position == "def"] <- "Defender"
  myData$Position[myData$Position == "Def"] <- "Defender"
  myData$Position[myData$Position == "defence"] <- "Defender"
  myData$Position[myData$Position == "Defence"] <- "Defender"
  myData$Position[myData$Position == "Defence and Forward"] <- "Defender"
  myData$Position[myData$Position == "defence/forward"] <- "Defender"
  myData$Position[myData$Position == "defence/midfield"] <- "Defender"
  myData$Position[myData$Position == "defenceman"] <- "Defender"
  myData$Position[myData$Position == "Defenceman"]<- "Defender"
  myData$Position[myData$Position == "Defencemen"]<- "Defender"
  myData$Position[myData$Position == "defender"]<- "Defender"
  myData$Position[myData$Position == "Defender"]<- "Defender"
  myData$Position[myData$Position == "Defender "]<- "Defender"
  myData$Position[myData$Position == "defense"]<- "Defender"
  myData$Position[myData$Position == "Defense"]<- "Defender"
  myData$Position[myData$Position == "defense "]<- "Defender"
  myData$Position[myData$Position == "Defense "]<- "Defender"
  myData$Position[myData$Position == "Defense -shoots Left"]<- "Defender"
  myData$Position[myData$Position == "Defense -shoots R"]<- "Defender"
  myData$Position[myData$Position == "Defense -shoots Right"]<- "Defender"
  myData$Position[myData$Position == "Defense  -shoots Right"]<- "Defender"
  myData$Position[myData$Position == "Defense  shoots L Dominant Eye R"]<- "Defender"
  myData$Position[myData$Position == "defense  shoots Left"]<- "Defender"
  myData$Position[myData$Position == "Defense shoots Left"]<- "Defender"
  myData$Position[myData$Position == "Defense, shoots  Right ,dominant hand R"]<- "Defender"
  myData$Position[myData$Position == "Defenseman"]<- "Defender"
  myData$Position[myData$Position == "Defenseman "]<- "Defender"
  myData$Position[myData$Position == "defensemen"]<- "Defender"
  myData$Position[myData$Position == "defensene"]<- "Defender"
  myData$Position[myData$Position == "defenseue"]<- "Defender"
  myData$Position[myData$Position == "defenseur"]<- "Defender"
  myData$Position[myData$Position == "Defenseur"]<- "Defender"
  myData$Position[myData$Position == "defenseur "]<- "Defender"
  myData$Position[myData$Position == "Defenseur "]<- "Defender"
  myData$Position[myData$Position == "defensive"]<- "Defender"
  myData$Position[myData$Position == "Defensive back"]<- "Defender"
  myData$Position[myData$Position == "Defensive Back"]<- "Defender"
  myData$Position[myData$Position == "Defensive End"]<- "Defender"
  myData$Position[myData$Position == "Defensive Line"]<- "Defender"
  myData$Position[myData$Position == "Defensive lineman"]<- "Defender"
  myData$Position[myData$Position == "Defensive Tackle"]<- "Defender"
  myData$Position[myData$Position == "defensman"]<- "Defender"
  myData$Position[myData$Position == "Defensw"]<- "Defender"
  myData$Position[myData$Position == "Defesne"]<- "Defender"
  myData$Position[myData$Position == "deffence"]<- "Defender"
  myData$Position[myData$Position == "deffenseur"]<- "Defender"
  myData$Position[myData$Position == "defnce"]<- "Defender"
  myData$Position[myData$Position == "derence"]<- "Defender"
  myData$Position[myData$Position == "DH/OUtfield"]<- "Defender"
  myData$Position[myData$Position == "DL"]<- "Defender"
  myData$Position[myData$Position == "DL/OL"]<- "Defender"
  myData$Position[myData$Position == "DLine"]<- "Defender"
  myData$Position[myData$Position == "DM"]<- "Defender"
  myData$Position[myData$Position == "dman"]<- "Defender"
  myData$Position[myData$Position == "DmSN"]<- "Defender"
  myData$Position[myData$Position == "double"]<- "Defender"
  myData$Position[myData$Position == "dt"]<- "Defender"
  myData$Position[myData$Position == "DT"]<- "Defender"
  myData$Position[myData$Position == "Everywhere"]<- "Multiple"
  myData$Position[myData$Position == "f"]<- "Forward"
  myData$Position[myData$Position == "F"]<- "Forward"
  myData$Position[myData$Position == "F "]<- "Forward"
  myData$Position[myData$Position == "F & D"]<- "Multiple"
  myData$Position[myData$Position == "F/D"]<- "Multiple"
  myData$Position[myData$Position == "face off"]<- "Other"
  myData$Position[myData$Position == "faceoff"]<- "Other"
  myData$Position[myData$Position == "FB"]<- "Infield"
  myData$Position[myData$Position == "first base"]<- "Infield"
  myData$Position[myData$Position == "flank"]<- "Forward"
  myData$Position[myData$Position == "Flank"]<- "Forward"
  myData$Position[myData$Position == "flank/8"]<- "Forward"
  myData$Position[myData$Position == "Flanker or Second row"]<- "Forward"
  myData$Position[myData$Position == "Flanker, second row"]<- "Forward"
  myData$Position[myData$Position == "Fly-Half"]<- "Back"
  myData$Position[myData$Position == "Fly half"]<- "Back"
  myData$Position[myData$Position == "Flyhalf"]<- "Back"
  myData$Position[myData$Position == "Flying"]<- "Forward"
  myData$Position[myData$Position == "Foreward"]<- "Forward"
  myData$Position[myData$Position == "forwaed"]<- "Forward"
  myData$Position[myData$Position == "forward"]<- "Forward"
  myData$Position[myData$Position == "Forward"]<- "Forward"
  myData$Position[myData$Position == "forward"]<- "Forward"
  myData$Position[myData$Position == "Forward"]<- "Forward"
  myData$Position[myData$Position == "Forward (Center/Right Wing)"]<- "Forward"
  myData$Position[myData$Position == "Forward (W)"]<- "Forward"
  myData$Position[myData$Position == "forward and defense"]<- "Multiple"
  myData$Position[myData$Position == "forward left wing"]<- "Forward"
  myData$Position[myData$Position == "Forward Right"]<- "Forward"
  myData$Position[myData$Position == "forward/centre"]<- "Forward"
  myData$Position[myData$Position == "forward/defence"]<- "Multiple"
  myData$Position[myData$Position == "Forward/Defence"]<- "Multiple"
  myData$Position[myData$Position == "forward/defense"]<- "Multiple"
  myData$Position[myData$Position == "forward/mid"]<- "Multiple"
  myData$Position[myData$Position == "forwarded"]<- "Forward"
  myData$Position[myData$Position == "Forwards"]<- "Forward"
  myData$Position[myData$Position == "forwatd"]<- "Forward"
  myData$Position[myData$Position == "foward"]<- "Forward"
  myData$Position[myData$Position == "Foward"]<- "Forward"
  myData$Position[myData$Position == "foward "]<- "Forward"
  myData$Position[myData$Position == "Full-Back"]<- "Offense"
  myData$Position[myData$Position == "Fullback"]<- "Offense"
  myData$Position[myData$Position == "Fullback or wing"]<- "Offense"
  myData$Position[myData$Position == "FW"]<- "Forward"
  myData$Position[myData$Position == "fwd"]<- "Forward"
  myData$Position[myData$Position == "Fwd"]<- "Forward"
  myData$Position[myData$Position == "fwd "]<- "Forward"
  myData$Position[myData$Position == "g"]<- "Goal"
  myData$Position[myData$Position == "G"]<- "Goal"
  myData$Position[myData$Position == "G "]<- "Goal"
  myData$Position[myData$Position == "Gardien"]<- "Defender"
  myData$Position[myData$Position == "Gaurd"]<- "Goal"
  myData$Position[myData$Position == "Gk"]<- "Goal"
  myData$Position[myData$Position == "GK"]<- "Goal"
  myData$Position[myData$Position == "Goaile"]<- "Goal"
  myData$Position[myData$Position == "Goal"]<- "Goal"
  myData$Position[myData$Position == "Goal keeper"]<- "Goal"
  myData$Position[myData$Position == "Goaler"]<- "Goal"
  myData$Position[myData$Position == "goalie"]<- "Goal"
  myData$Position[myData$Position == "Goalie"]<- "Goal"
  myData$Position[myData$Position == "goalie "]<- "Goal"
  myData$Position[myData$Position == "Goalie "]<- "Goal"
  myData$Position[myData$Position == "Goalkeeper"]<- "Goal"
  myData$Position[myData$Position == "goaltender"]<- "Goal"
  myData$Position[myData$Position == "Goaltender"]<- "Goal"
  myData$Position[myData$Position == "goaltender-shoots Left"]<- "Goal"
  myData$Position[myData$Position == "Goaltender "]<- "Goal"
  myData$Position[myData$Position == "Goaltender -shoots Left"]<- "Goal"
  myData$Position[myData$Position == "golaie"]<- "Goal"
  myData$Position[myData$Position == "golie"]<- "Goal"
  myData$Position[myData$Position == "guard"]<- "Guard"
  myData$Position[myData$Position == "Guard"]<- "Guard"
  myData$Position[myData$Position == "hook"]<- "Forward"
  myData$Position[myData$Position == "Hook"]<- "Forward"
  myData$Position[myData$Position == "hooker"]<- "Forward"
  myData$Position[myData$Position == "Hooker"]<- "Forward"
  myData$Position[myData$Position == "hybrid"]<- "Other"
  myData$Position[myData$Position == "In field"]<- "Infield"
  myData$Position[myData$Position == "Infield"]<- "Infield"
  myData$Position[myData$Position == "Inside Linebacker"]<- "Defender"
  myData$Position[myData$Position == "inside/ outside center"]<- "Back"
  myData$Position[myData$Position == "jfhvjvjvjv"]<- "Offense"
  myData$Position[myData$Position == "K"]<- "Goal"
  myData$Position[myData$Position == "keeper"]<- "Goal"
  myData$Position[myData$Position == "Keeper"]<- "Goal"
  myData$Position[myData$Position == "Kicker"]<- "Kicker"
  myData$Position[myData$Position == "L"]<- "Defender"
  myData$Position[myData$Position == "LB"]<- "Defender"
  myData$Position[myData$Position == "LC"]<- "Defender"
  myData$Position[myData$Position == "LD"]<- "Defender"
  myData$Position[myData$Position == "left-wing"]<- "Forward"
  myData$Position[myData$Position == "left / outside hitter"]<- "Offense"
  myData$Position[myData$Position == "Left back"]<- "Defender"
  myData$Position[myData$Position == "Left Defence"]<- "Defender"
  myData$Position[myData$Position == "left defense"]<- "Defender"
  myData$Position[myData$Position == "left Defense"]<- "Defender"
  myData$Position[myData$Position == "Left field"]<- "Outfield"
  myData$Position[myData$Position == "Left side"]<- "Other"
  myData$Position[myData$Position == "left side"]<- "Other"
  myData$Position[myData$Position == "left wing"]<- "Forward"
  myData$Position[myData$Position == "Left wing"]<- "Forward"
  myData$Position[myData$Position == "Left Wing"]<- "Forward"
  myData$Position[myData$Position == "Left Wing-shoots Left"]<- "Forward"
  myData$Position[myData$Position == "left wing"]<- "Forward"
  myData$Position[myData$Position == "Left Wing"]<- "Forward"
  myData$Position[myData$Position == "Left Wing - shoots Left"]<- "Forward"
  myData$Position[myData$Position == "Left Wing -shoots Left"]<- "Forward"
  myData$Position[myData$Position == "Left Wing  - shoots  Left"]<- "Forward"
  myData$Position[myData$Position == "left wing/center"]<- "Forward"
  myData$Position[myData$Position == "left winger"]<- "Forward"
  myData$Position[myData$Position == "Left winger"]<- "Forward"
  myData$Position[myData$Position == "left winger"]<- "Forward"
  myData$Position[myData$Position == "Left Winger"]<- "Forward"
  myData$Position[myData$Position == "Left/Right Wing"]<- "Forward"
  myData$Position[myData$Position == "leftside"]<- "Other"
  myData$Position[myData$Position == "Leftside"]<- "Other"
  myData$Position[myData$Position == "leftwing"]<- "Forward"
  myData$Position[myData$Position == "LF"]<- "Outfield"
  myData$Position[myData$Position == "libero"]<- "Other"
  myData$Position[myData$Position == "Libero"]<- "Other"
  myData$Position[myData$Position == "Linbacker/Runningback"]<- "Multiple"
  myData$Position[myData$Position == "linebacker"]<- "Defender"
  myData$Position[myData$Position == "Linebacker"]<- "Defender"
  myData$Position[myData$Position == "Linebacker, Running Back"]<- "Multiple"
  myData$Position[myData$Position == "lock"]<- "Other"
  myData$Position[myData$Position == "Lock"]<- "Other"
  myData$Position[myData$Position == "Long Snapper"]<- "Offense"
  myData$Position[myData$Position == "LS"]<- "Offense"
  myData$Position[myData$Position == "lw"]<- "Forward"
  myData$Position[myData$Position == "Lw"]<- "Forward"
  myData$Position[myData$Position == "LW"]<- "Forward"
  myData$Position[myData$Position == "Lw Or C Either Or"]<- "Forward"
  myData$Position[myData$Position == "Lw/center"]<- "Forward"
  myData$Position[myData$Position == "Lw/Center"]<- "Forward"
  myData$Position[myData$Position == "Male"]<- "Other"
  myData$Position[myData$Position == "MB"]<- "Other"
  myData$Position[myData$Position == "Mid"]<- "Midfielder"
  myData$Position[myData$Position == "MID"]<- "Midfielder"
  myData$Position[myData$Position == "mid and defensive"]<- "Multiple"
  myData$Position[myData$Position == "mid field"]<- "Midfielder"
  myData$Position[myData$Position == "Mid/Defence"]<- "Multiple"
  myData$Position[myData$Position == "middle"]<- "Other"
  myData$Position[myData$Position == "Middle"]<- "Other"
  myData$Position[myData$Position == "Middle Blocker"]<- "Defender"
  myData$Position[myData$Position == "Middle Linebacker"]<- "Defender"
  myData$Position[myData$Position == "Middle/Opp"]<- "Multiple"
  myData$Position[myData$Position == "Middle/Outside"]<- "Multiple"
  myData$Position[myData$Position == "midfield"]<- "Midfielder"
  myData$Position[myData$Position == "Midfield"]<- "Midfielder"
  myData$Position[myData$Position == "Midfielder"]<- "Midfielder"
  myData$Position[myData$Position == "midfielder"]<- "Midfielder"
  myData$Position[myData$Position == "nor miss"]<- "Other"
  myData$Position[myData$Position == "O"]<- "Offense"
  myData$Position[myData$Position == "OF"]<- "Offense"
  myData$Position[myData$Position == "offence"]<- "Offense"
  myData$Position[myData$Position == "Offence"]<- "Offense"
  myData$Position[myData$Position == "offense"]<- "Offense"
  myData$Position[myData$Position == "Offensive Guard"]<- "Offense"
  myData$Position[myData$Position == "Offensive Tackle"]<- "Offense"
  myData$Position[myData$Position == "OH"]<- "Offense"
  myData$Position[myData$Position == "ol"]<- "Offense"
  myData$Position[myData$Position == "OL"]<- "Offense"
  myData$Position[myData$Position == "oline"]<- "Offense"
  myData$Position[myData$Position == "open side flank"]<- "Forward"
  myData$Position[myData$Position == "Opposite hitter"]<- "Other"
  myData$Position[myData$Position == "Out fielder"]<- "Outfield"
  myData$Position[myData$Position == "outfield"]<- "Outfield"
  myData$Position[myData$Position == "Outfield"]<- "Outfield"
  myData$Position[myData$Position == "Outfield "]<- "Outfield"
  myData$Position[myData$Position == "Outfield/Pitcher"]<- "Multiple"
  myData$Position[myData$Position == "Outfielder"]<- "Outfield"
  myData$Position[myData$Position == "Outside"]<- "Other"
  myData$Position[myData$Position == "Outside back"]<- "Defender"
  myData$Position[myData$Position == "Outside Centre"]<- "Defender"
  myData$Position[myData$Position == "Outside Hitter"]<- "Other"
  myData$Position[myData$Position == "Outside linebacker"]<- "Defender"
  myData$Position[myData$Position == "pg"]<- "Guard"
  myData$Position[myData$Position == "PG"]<- "Guard"
  myData$Position[myData$Position == "pitcher"]<- "Pitcher"
  myData$Position[myData$Position == "Pitcher"]<- "Pitcher"
  myData$Position[myData$Position == "Pitcher/1st base"]<- "Multiple"
  myData$Position[myData$Position == "Pitcher/infield"]<- "Multiple"
  myData$Position[myData$Position == "Player"]<- "Other"
  myData$Position[myData$Position == "power forward"]<- "Forward"
  myData$Position[myData$Position == "Power forward & Centre"]<- "Forward"
  myData$Position[myData$Position == "Prop"]<- "Forward"
  myData$Position[myData$Position == "prop/lock"]<- "Forward"
  myData$Position[myData$Position == "Punter"]<- "Kicker"
  myData$Position[myData$Position == "Qb"]<- "Quarterback"
  myData$Position[myData$Position == "QB"]<- "Quarterback"
  myData$Position[myData$Position == "Quarterback"]<- "Quarterback"
  myData$Position[myData$Position == "R"]<- "Other"
  myData$Position[myData$Position == "R D"]<- "Defender"
  myData$Position[myData$Position == "R wing"]<- "Forward"
  myData$Position[myData$Position == "RB"]<- "Offense"
  myData$Position[myData$Position == "RB/WR"]<- "Offense"
  myData$Position[myData$Position == "RC"]<- "Forward"
  myData$Position[myData$Position == "RD"]<- "Defender"
  myData$Position[myData$Position == "Receiver"]<- "Offense"
  myData$Position[myData$Position == "Reciver"]<- "Offense"
  myData$Position[myData$Position == "RF"]<- "Forward"
  myData$Position[myData$Position == "right"]<- "Other"
  myData$Position[myData$Position == "Right"]<- "Other"
  myData$Position[myData$Position == "right "]<- "Other"
  myData$Position[myData$Position == "Right "]<- "Other"
  myData$Position[myData$Position == "right back"]<- "Defender"
  myData$Position[myData$Position == "right defence"]<- "Defender"
  myData$Position[myData$Position == "Right defence"]<- "Defender"
  myData$Position[myData$Position == "Right Defence"]<- "Defender"
  myData$Position[myData$Position == "Right Defenceman"]<- "Defender"
  myData$Position[myData$Position == "Right Defencemen"]<- "Defender"
  myData$Position[myData$Position == "Right Defense"]<- "Defender"
  myData$Position[myData$Position == "right defense"]<- "Defender"
  myData$Position[myData$Position == "Right field"]<- "Other"
  myData$Position[myData$Position == "right side"]<- "Other"
  myData$Position[myData$Position == "Right Side"]<- "Other"
  myData$Position[myData$Position == "right wing"]<- "Forward"
  myData$Position[myData$Position == "Right wing"]<- "Forward"
  myData$Position[myData$Position == "Right Wing"]<- "Forward"
  myData$Position[myData$Position == "right wing "]<- "Forward"
  myData$Position[myData$Position == "Right wing "]<- "Forward"
  myData$Position[myData$Position == "Right Wing "]<- "Forward"
  myData$Position[myData$Position == "Right Wing - shoots R"]<- "Forward"
  myData$Position[myData$Position == "Right Wing -shoots Left"]<- "Forward"
  myData$Position[myData$Position == "Right Wing  - shoots Right"]<- "Forward"
  myData$Position[myData$Position == "right wing and center"]<- "Forward"
  myData$Position[myData$Position == "right wing forward"]<- "Forward"
  myData$Position[myData$Position == "rightwing"]<- "Forward"
  myData$Position[myData$Position == "Righty Offense"]<- "Forward"
  myData$Position[myData$Position == "rigt wing"]<- "Forward"
  myData$Position[myData$Position == "RS"]<- "Other"
  myData$Position[myData$Position == "rught wing"]<- "Forward"
  myData$Position[myData$Position == "Running back"]<- "Offense"
  myData$Position[myData$Position == "Running Back"]<- "Offense"
  myData$Position[myData$Position == "Running Back, Cornerback"]<- "Multiple"
  myData$Position[myData$Position == "rw"]<- "Forward"
  myData$Position[myData$Position == "Rw"]<- "Forward"
  myData$Position[myData$Position == "RW"]<- "Forward"
  myData$Position[myData$Position == "Rw "]<- "Forward"
  myData$Position[myData$Position == "Rw/C"]<- "Forward"
  myData$Position[myData$Position == "RW/LW"]<- "Forward"
  myData$Position[myData$Position == "S"]<- "Other"
  myData$Position[myData$Position == "S/L"]<- "Other"
  myData$Position[myData$Position == "scrum half"]<- "Back"
  myData$Position[myData$Position == "Scrum half"]<- "Back"
  myData$Position[myData$Position == "Scrum Half"]<- "Back"
  myData$Position[myData$Position == "Second base"]<- "Infield"
  myData$Position[myData$Position == "Second Row"]<- "Back"
  myData$Position[myData$Position == "Setter"]<- "Other"
  myData$Position[myData$Position == "shooting guard"]<- "Guard"
  myData$Position[myData$Position == "shortstop"]<- "Infield"
  myData$Position[myData$Position == "Shortstop"]<- "Infield"
  myData$Position[myData$Position == "Slotback"]<- "Offense"
  myData$Position[myData$Position == "striker"]<- "Offense"
  myData$Position[myData$Position == "Striker"]<- "Offense"
  myData$Position[myData$Position == "Strong Safety"]<- "Defender"
  myData$Position[myData$Position == "T"]<- "Other"
  myData$Position[myData$Position == "Tackles"]<- "Offense"
  myData$Position[myData$Position == "TE"]<- "Offense"
  myData$Position[myData$Position == "Third base"]<- "Infield"
  myData$Position[myData$Position == "Transition"]<- "Other"
  myData$Position[myData$Position == "Utilitie"]<- "Other"
  myData$Position[myData$Position == "W"]<- "Offense"
  myData$Position[myData$Position == "Wide Receiver"]<- "Offense"
  myData$Position[myData$Position == "wide reciever"]<- "Offense"
  myData$Position[myData$Position == "Wide Reciever"]<- "Offense"
  myData$Position[myData$Position == "wing"]<- "Forward"
  myData$Position[myData$Position == "Wing"]<- "Forward"
  myData$Position[myData$Position == "wing "]<- "Forward"
  myData$Position[myData$Position == "Wing "]<- "Forward"
  myData$Position[myData$Position == "wing or center"]<- "Forward"
  myData$Position[myData$Position == "Wing/Center"]<- "Forward"
  myData$Position[myData$Position == "winger"]<- "Forward"
  myData$Position[myData$Position == "Winger"]<- "Forward"
  myData$Position[myData$Position == "Winger/forward"]<- "Forward"
  myData$Position[myData$Position == "winger/striker"]<- "Forward"
  myData$Position[myData$Position == "WR"]<- "Forward"
  myData$Position[myData$Position == "WR/DB"]<- "Multiple"
  j=1
}


#The output should show (in the bottom) 20 unique values
unique(myData$Sport)

#The output should show (in the bottom) 15 unique values
unique(myData$Position)


#Remove '#' and run the lines 526-527 if you would like to view the frequency table of variables Sport and Position respectively.
#View(table(myData$Sport))
#View(table(myData$Position))

#---------------------x-----------------------------2--------------------------------x-------------------------
  
k=0
while (k < 1) {
  idData_o <- subset(myData, select = c("Org.ID"))
  setDT(idData_o)[, 'OrgID-Mapped' := .GRP, by = 'Org.ID']
  idData_o <- idData_o[ ! duplicated( idData_o) , ]
  idData_o <- as.data.frame(idData_o)
  
  idData_t <- subset(myData, select = c("Team.ID"))
  setDT(idData_t)[, 'TeamID-Mapped' := .GRP, by = 'Team.ID']
  idData_t <- idData_t[ ! duplicated( idData_t) , ]
  idData_t <- as.data.frame(idData_t)
  
  idData_a <- subset(myData, select = c("Athlete.ID"))
  setDT(idData_a)[, 'AthleteID-Mapped' := .GRP, by = 'Athlete.ID']
  idData_a <- idData_a[ ! duplicated( idData_a) , ]
  idData_a <- as.data.frame(idData_a)
  
  idData_e <- subset(myData, select = c("Examiner.ID"))
  setDT(idData_e)[, 'ExaminerID-Mapped' := .GRP, by = 'Examiner.ID']
  idData_e <- idData_e[ ! duplicated( idData_e) , ]
  idData_e <- as.data.frame(idData_e)
  
  idData_as <- subset(myData, select = c("Assessment.ID"))
  setDT(idData_as)[, 'Assessment.ID-Mapped' := .GRP, by = 'Assessment.ID']
  idData_as <- idData_as[ ! duplicated( idData_as) , ]
  idData_as <- as.data.frame(idData_as)
  
  idData_tc <- subset(myData, select = c("Test.collection"))
  setDT(idData_tc)[, 'Test.collection-Mapped' := .GRP, by = 'Test.collection']
  idData_tc <- idData_tc[ ! duplicated( idData_tc) , ]
  idData_tc <- as.data.frame(idData_tc)
  
  idData_rt <- subset(myData, select = c("RTP.ID"))
  setDT(idData_rt)[, 'RTP.ID-Mapped' := .GRP, by = 'RTP.ID']
  idData_rt <- idData_rt[ ! duplicated( idData_rt) , ]
  idData_rt <- as.data.frame(idData_rt)
  
  write.csv(idData_o, "OrgID-Map.csv")
  write.csv(idData_t, "TeamID-Map.csv")
  write.csv(idData_a, "AthleteID-Map.csv")
  write.csv(idData_e, "ExaminerID-Map.csv")
  write.csv(idData_as, "AssessmentID-Mapped.csv")
  write.csv(idData_tc, "TestCollection-Mapped.csv")
  write.csv(idData_rt, "RTPID-Mapped.csv")
  k=1
}

#---------------------x-----------------------------3--------------------------------x-------------------------

#Generating mapping of artificial IDs
setDT(myData)[, 'Org.ID' := .GRP, by = 'Org.ID']
setDT(myData)[, 'Team.ID' := .GRP, by = 'Team.ID']
setDT(myData)[, 'Athlete.ID' := .GRP, by = 'Athlete.ID']
setDT(myData)[, 'Examiner.ID' := .GRP, by = 'Examiner.ID']
setDT(myData)[, 'Assessment.ID' := .GRP, by = 'Assessment.ID']
setDT(myData)[, 'Test.collection' := .GRP, by = 'Test.collection']
setDT(myData)[, 'RTP.ID' := .GRP, by = 'RTP.ID']

myData<-as.data.frame(myData)


# converting Birthdate to numeric
myData$Birthdate <-as.Date(paste('01/', myData$Birthdate), format='%d/%m/%Y', origin="1970-01-01")
myData$Birthdate<-as.numeric(myData$Birthdate)

#Convert Injury date to numeric
myData$Injury.Date<-strptime(paste0(as.character(myData$Injury.Date)," 00:00"), format = "%d/%m/%Y %H:%M")
str(myData$Injury.Date)
myData$Injury.Date<-as.numeric(myData$Injury.Date)

#Convert Assessment date to numeric
myData$Assessment.Date<-strptime(paste0(as.character(myData$Assessment.Date)," 00:00"), format = "%d/%m/%Y %H:%M")
str(myData$Assessment.Date)
myData$Assessment.Date<-as.numeric(myData$Assessment.Date)

#Convert Fatigue to numeric
myData$Fatigue <- as.numeric(myData$Fatigue)
#myData$Fatigue <- as.numeric(as.character(myData$Fatigue))

str(myData)
#View(myData)


myData$Team.ID <- as.numeric(myData$Team.ID)
myData$Org.ID <- as.numeric(myData$Org.ID)
myData$Athlete.ID <- as.numeric(myData$Athlete.ID)
myData$Examiner.ID <- as.numeric(myData$Examiner.ID)
myData$Assessment.ID <- as.numeric(myData$Assessment.ID)
myData$Test.collection <- as.numeric(myData$Test.collection)
myData$RTP.ID <- as.numeric(myData$RTP.ID)

rules.list <- list(
  Injury.Date = "Test.Type ==  'Baseline'",
  Concussion.Diagnosed.Decision = "Test.Type ==  'Baseline' || Test.Type ==  'RTP' || Test.Type ==  'Athlete Self-Test'",
  Removed.from.Play = "Test.Type ==  'Baseline'")

rules.value.list <- list(
  Injury.Date = NA,
  Concussion.Diagnosed.Decision = NA,
  Removed.from.Play = NA)

str(myData)

myseed=1337

#synthing
ptm <- proc.time()
synth.obj <- syn(myData, method = "cart",rules = rules.list, rvalues = rules.value.list, seed = myseed, maxfaclevels = 430)
proc.time() - ptm
syn1<-synth.obj$syn

syn1$Birthdate<-as.Date(syn1$Birthdate ,origin="1970-01-01")
syn1$Injury.Date <- as.POSIXct(syn1$Injury.Date, origin="1970-01-01")
syn1$Assessment.Date <- as.POSIXct(syn1$Assessment.Date, origin="1970-01-01")
str(syn1)
View(syn1)

compare(synth.obj,myData, stat = "counts")

#stats on the synth'd dataset
summary(synth.obj)
synth.obj$predictor.matrix
synth.obj$method

#export to csv
write.csv(syn1, "Synthesized_Data.csv", row.names = FALSE)

