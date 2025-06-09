package org.udesa.tp4.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.service.UnoService;

import java.util.List;
import java.util.UUID;

@Controller
public class UnoController {
    @Autowired UnoService unoService;
//    @GetMapping ("/")
//    public String saludo() {return "index";}
//    public String saludo() {return "hola luidolacrack";}

    @GetMapping("/hola")
    public ResponseEntity<String> holaMundo() {
        return new ResponseEntity<>("respuesta a Hola Mundo", HttpStatus.OK);
    }
    @PostMapping("newmatch") public ResponseEntity newMatch( @RequestParam List<String> players ) {
        return ResponseEntity.ok(unoService.newMatch(players));}

    @PostMapping("play/{matchId}/{player}") public ResponseEntity
    play(@PathVariable UUID matchId, @PathVariable String player, @RequestBody JsonCard card ) {
        return ResponseEntity.ok(unoService.play(matchId, player, card));
    }

    //@PostMapping("draw/{matchId}/{player}") public ResponseEntity drawCard( @PathVariable UUID matchId, @PathVariable String player ) {}
    @GetMapping("activecard/{matchId}") public ResponseEntity activeCard( @PathVariable UUID matchId ) {
        return ResponseEntity.ok(unoService.activeCard(matchId));
    }
//    @GetMapping("playerhand/{matchId}") public ResponseEntity playerHand( @PathVariable UUID matchId ) {}

}
