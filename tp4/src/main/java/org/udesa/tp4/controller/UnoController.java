package org.udesa.tp4.controller;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
public class UnoController {
    @GetMapping ("/")
    public String saludo() {return "index";}
//    public String saludo() {return "hola luidolacrack";}

}
