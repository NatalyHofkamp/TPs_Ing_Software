package org.udesa.tp4.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.service.UnoService;

import java.util.List;
import java.util.UUID;
//a
@Controller
public class UnoController {
    @Autowired UnoService unoService;
//    @GetMapping ("/")
//    public String saludo() {return "index";}
//    public String saludo() {return "hola luidolacrack";}

    private static final Logger logger = LoggerFactory.getLogger(UnoController.class);

    @GetMapping("/")
    public String saludo() {
        return "index";  // va a buscar templates/index.html
    }
    @ExceptionHandler(RuntimeException.class)
    public ResponseEntity<String> handleRuntimeException(RuntimeException ex) {
//        logger.error("Error interno en la aplicación", ex);  // Log completo
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("Error interno: " + ex.getMessage());
    }


    @ExceptionHandler(IllegalArgumentException.class)
    public ResponseEntity<String> handleIllegalArgumentException(IllegalArgumentException ex) {
        return ResponseEntity.badRequest().body("Error en la petición: " + ex.getMessage());
    }
    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ResponseEntity<String> handleJsonParseError(HttpMessageNotReadableException ex) {
        return ResponseEntity.badRequest().body("Formato de JSON inválido: " + ex.getMostSpecificCause().getMessage());
    }


    @PostMapping("newmatch") public ResponseEntity newMatch( @RequestParam List<String> players ) {
        return ResponseEntity.ok(unoService.newMatch(players));}

    @PostMapping("play/{matchId}/{player}") public ResponseEntity
    play(@PathVariable UUID matchId, @PathVariable String player, @RequestBody JsonCard card ) {
        UUID result = unoService.play(matchId, player, card);
//        if (result == null) {
//            // Aquí no se lanza excepción, sino que se retorna 400 con mensaje
//            return ResponseEntity.badRequest().body("Invalid play");
//        }
        return ResponseEntity.ok(result);
    }
    @GetMapping("newmatch")
    public ResponseEntity testNewMatch() {
        List<String> testPlayers = List.of("luidolacrack", "botcito");
        return ResponseEntity.ok(unoService.newMatch(testPlayers));
    }


    @PostMapping("draw/{matchId}/{player}") public ResponseEntity
    drawCard( @PathVariable UUID matchId, @PathVariable String player ) {
        return ResponseEntity.ok(unoService.drawCard(matchId, player));
    }
    @GetMapping("activecard/{matchId}") public ResponseEntity activeCard( @PathVariable UUID matchId ) {
        return ResponseEntity.ok(unoService.activeCard(matchId).asJson());
    }
    @GetMapping("playerhand/{matchId}") public ResponseEntity playerHand( @PathVariable UUID matchId ) {
        return ResponseEntity.ok(unoService.playerHand(matchId));
    }

}
