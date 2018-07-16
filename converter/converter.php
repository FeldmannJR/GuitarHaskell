<?php
//https://tonejs.github.io/MidiConvert/ = Site para converter arquivos .mid(Guitar hero salva as musicas em mid assim fica facil de converter) para json
//https://chorus.fightthe.pw/ = Site para baixar musicas do guitar hero

//Esse script converte o json gerado pelo site para um código em haskell da musica
//Por que não fiz em haskell? Ia demorar 5 anos pra fazer isso enquanto em php faço em 2min

  $delay =0;
  if(empty($argv[1])){
    echo "Use php converter.php NomeDoArquivo.json delay";
    echo "\nUse o site para converter mid para json https://tonejs.github.io/MidiConvert/";
    return;
  }
  if(!empty($argv[2])){
    $delay = floatval($argv[2]);
  }
  $fname = dirname(__FILE__)."/musicas/".$argv[1];

  if(!file_exists($fname)){
    echo "Não foi achado o arquivo ".$fname." na pasta de musicas!";
    return;
  }
  $json = json_decode(file_get_contents($fname));

  $tracks = $json->tracks;
  foreach($tracks as $tr){
    if($tr->length>0){
      $track = $tr;
      break;
    }
  }
  if(!isset($track)){
    return;
  }
  $notes = $track->notes;
  $first = true;
  $conv = array("96" => 0,"97" => 1,"98" => 2,"99" => 3, "102" => 4);
  echo "[";
  foreach ($notes as $n) {
    if(array_key_exists($n->midi,$conv)){
      echo "\n".($first?" ":" ,")." (Not ".(floatval($n->time)+$delay)." ".$conv[$n->midi]." ".$n->duration.")";
      $first = false;
    }
  }
  echo "]";

//96
//97
//98
//99
//102
?>
