<?php
//https://tonejs.github.io/MidiConvert/
//https://chorus.fightthe.pw/
  $json = json_decode(file_get_contents("./ex.json"));

  $tracks = $json->tracks[0];
  $notes = $tracks->notes;
  $conv = array("96" => 0,"97" => 1,"98" => 2,"99" => 3, "102" => 4);

  echo "[";
  foreach ($notes as $n) {
    if(array_key_exists($n->midi,$conv)){
      echo "\n , (".$n->time.", ".$conv[$n->midi].")";
    }
  }
  echo "]";

//96
//97
//98
//99
//102
?>
