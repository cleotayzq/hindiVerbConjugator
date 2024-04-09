# function to conjugate hona in past, present, and future tense ----
conjugateHona <- function(conjugationType, tense) {
  
  # types of conjugations: c("sing_1_m", "sing_23_m", "plur_2_m", "plur_123_m", "sing_1_f", "sing_23_f", "plur_2_f", "plur_123_f")
  # types of tenses: c("past", "present", "future") // special case: past/future indefinite
  
  number = strsplit(conjugationType, "_")[[1]][1]
  person = strsplit(conjugationType, "_")[[1]][2]
  gender = strsplit(conjugationType, "_")[[1]][3]
  
  if (tense == "present") {
    if (person == "1") {
      rom = "hū̃"
      dev = "हूँ"
    } else if (person == "23") {
      rom = "hai"
      dev = "है"
    } else if (person == "2") {
      rom = "ho"
      dev = "हो"
    } else if (person == "123") {
      rom = "hain"
      dev = "हैं"
    }
    
  } else if (tense == "future") {
    # in two parts e.g. hoga
    if (person == "1") {
      rom_first = "hūṅ"
      dev_first = "हूँ"
    } else if (person == "23" | person == "2") {
      rom_first = "ho"
      dev_first = "हो"
    } else if (person == "123") {
      rom_first = "hoṅ"
      dev_first = "हों"
    }
    if (gender == "f") {
      rom_second = "gī"
      dev_second = "गी"
    } else if (gender == "m") {
      if (number == "sing") {
        rom_second = "gā"
        dev_second = "गा"
      } else if (number == "plur") {
        rom_second = "ge"
        dev_second = "गे"
      }
    }
    rom = paste0(rom_first, rom_second)
    dev = paste0(dev_first, dev_second)
    
  } else if (tense == "past") {
    if (gender == "m") {
      if (number == "sing") {
        rom = "thā"
        dev = "था"
      } else if (number == "plur") {
        rom = "the"
        dev = "थे"        
      }
    } else if (gender == "f") {
      if (number == "sing" | person == "2") {
        rom = "thī"
        dev = "थी"   
      } else if (person == "123") {
        rom = "thī̃"
        dev = "थीं" 
      }
    }
  } else if (tense == "indefinite") { # for past/future indefinite
    rom = ""
    dev = ""
  }
  
  return(c(dev, rom))
}

# function to conjugate aspect as perfect, imperfect, or continuous ----
conjugateAspect <- function(conjugationType, aspect, vowel) { # also transitivity
  
  # types of conjugations: c("sing_1_m", "sing_23_m", "plur_2_m", "plur_123_m", "sing_1_f", "sing_23_f", "plur_2_f", "plur_123_f")
  # types of tenses: c("perfect", "imperfect", "continuous") special: future
  # special cases: past + perfect = past perfect, indefinite + perfect = past indefinite, indefinite + future = future indefinite
  
  number = strsplit(conjugationType, "_")[[1]][1]
  person = strsplit(conjugationType, "_")[[1]][2]
  gender = strsplit(conjugationType, "_")[[1]][3]
  
  if (aspect == "imperfect" | aspect == "continuous" | aspect == "future") {
    
    if (gender == "f") {
      rom_second = "ī"
      dev_second = "ी"
    } else if (gender == "m") {
      if (number == "sing") {
        rom_second = "ā"
        dev_second = "ा"
      } else if (number == "plur") {
        rom_second = "e"
        dev_second = "े"
      }
    }
    
    if (aspect == "imperfect") {
      rom_first = "t"
      dev_first = "त"
    } else if (aspect == "continuous") {
      rom_first = " rah"
      dev_first = " रह"
    
    } else if (aspect == "future") {
      if (person == "1") {
        rom_first = "ūṅg"
        if (vowel == TRUE) {
          dev_first = "ऊँग"
        } else {
          dev_first = "ूँग"
        }
      } else if (person == "23") {
        if (vowel == TRUE) {
          rom_first = "yeg"
          dev_first = "येग"
        } else {
          rom_first = "eg"
          dev_first = "ेग"
        }
      } else if (person == "2") {
        rom_first = "og"
        if (vowel == TRUE) {
          dev_first = "ओग"
        } else {
          dev_first = "ोग"
        }
      } else if (person == "123") {
        if (vowel == TRUE) {
          rom_first = "yeṅg"
          dev_first = "येंग"
        } else {
          rom_first = "eṅg"
          dev_first = "ेंग"
        }
      }
    }
    
    rom = paste0(rom_first, rom_second)
    dev = paste0(dev_first, dev_second)
    
  } else if (aspect == "perfect") {
    
    if (vowel == TRUE) {
      if (gender == "m") {
        if (number == "sing") {
          rom = "yā"
          dev = "या"
        } else if (number == "plur") {
          rom = "ye"
          dev = "ये" 
        }
      } else if (gender == "f") {
        if (number == "sing" | person == "2") {
          rom = "yī"
          dev = "यी"   
        } else if (person == "123") {
          rom = "yī̃"
          dev = "यीं"
        }
      }
    } else {
      if (gender == "m") {
        if (number == "sing") {
          rom = "ā"
          dev = "ा"
        } else if (number == "plur") {
          rom = "e"
          dev = "े"        
        }
      } else if (gender == "f") {
        if (number == "sing" | person == "2") {
          rom = "ī"
          dev = "ी"   
        } else if (person == "123") {
          rom = "ī̃"
          dev = "ीं" 
        }
      }
    }
  }
  
  return(c(dev, rom))
}

# function to create table type 1 ----
createTable1 <- function(dev, rom, tense, aspect, vowel) {
  
  # tense is either present or future for table type 1
  # aspect is indefinite, perfect, imperfect, continuous for table type 1
  
  conjugationsType1 <- c("sing_1_m", "sing_23_m", "plur_2_m", "plur_123_m", "sing_1_f", "sing_23_f", "plur_2_f", "plur_123_f")
  honaList1 <- list()
  aspectList1 <- list()
  pronounList1 <- list()
  
  for (conjugation in conjugationsType1) {
    honaList1[[conjugation]] <- conjugateHona(conjugation, tense)
    aspectList1[[conjugation]] <- conjugateAspect(conjugation, aspect, vowel)
    
    dev_string <- paste0(dev, aspectList1[[conjugation]][1], " ", honaList1[[conjugation]][1])
    rom_string <- paste0(rom, aspectList1[[conjugation]][2], " ", honaList1[[conjugation]][2])
    
    pronounList1[[conjugation]] <- paste0(dev_string, "<br>", rom_string)
  }
  
  htmlTable <- paste(
    '<table class="table table-bordered">
      <tr>
        <td colspan="8">Verb agrees with <b>subject</b></td>
      </tr>
      <tr>
        <td style="border: none;"></td>
        <td colspan="3"><b>Singular</b></td>
        <td colspan="4"><b>Plural</b></td>
      </tr>
      <tr>
        <td style="border: none; font-weight: bold;">Subject</td>
        <td><b>मैं</b></td>
        <td><b>तू</b></td>
        <td><b>यह/वह</b></td>
        <td><b>तुम</b></td>
        <td><b>आप</b></td>
        <td><b>हम</b></td>
        <td><b>ये/वे</b></td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Masc</b></td>
        <td>', pronounList1["sing_1_m"], '</td>
        <td colspan="2">', pronounList1["sing_23_m"], '</td>
        <td>', pronounList1["plur_2_m"], '</td>
        <td colspan="3">', pronounList1["plur_123_m"], '</td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Fem</b></td>
        <td>', pronounList1["sing_1_f"], '</td>
        <td colspan="2">', pronounList1["sing_23_f"], '</td>
        <td>', pronounList1["plur_2_f"], '</td>
        <td colspan="3">', pronounList1["plur_123_f"], '</td>
      </tr>
    </table>'
  )
  
  return(htmlTable)
}

createTable2 <- function(dev, rom, tense, aspect, vowel) {
  
  # tense is either present or future for table type 2
  # aspect is indefinite, perfect, imperfect, continuous for table type 2
  
  conjugationsType2 <- c("sing_23_m", "plur_123_m", "sing_23_f", "plur_123_f")
  honaList2 <- list()
  aspectList2 <- list()
  pronounList2 <- list()
  
  for (conjugation in conjugationsType2) {
    honaList2[[conjugation]] <- conjugateHona(conjugation, tense)
    aspectList2[[conjugation]] <- conjugateAspect(conjugation, aspect, vowel)
    
    dev_string <- paste0(dev, aspectList2[[conjugation]][1], " ", honaList2[[conjugation]][1])
    rom_string <- paste0(rom, aspectList2[[conjugation]][2], " ", honaList2[[conjugation]][2])
    
    pronounList2[[conjugation]] <- paste0(dev_string, "<br>", rom_string)
  }
  
  htmlTable <- paste(
    '<table class="table table-bordered">
      <tr>
        <td colspan="8">Verb agrees with <b>subject</b></td>
      </tr>
      <tr>
        <td style="border: none;"></td>
        <td colspan="3"><b>Singular</b></td>
        <td colspan="4"><b>Plural</b></td>
      </tr>
      <tr>
        <td style="border: none; font-weight: bold;">Subject</td>
        <td><b>मैं</b></td>
        <td><b>तू</b></td>
        <td><b>यह/वह</b></td>
        <td><b>तुम</b></td>
        <td><b>आप</b></td>
        <td><b>हम</b></td>
        <td><b>ये/वे</b></td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Masc</b></td>
        <td colspan="3">', pronounList2["sing_23_m"], '</td>
        <td colspan="4">', pronounList2["plur_123_m"], '</td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Fem</b></td>
        <td colspan="4">', pronounList2["sing_23_f"], '</td>
        <td colspan="3">', pronounList2["plur_123_f"], '</td>
      </tr>
    </table>'
  )
  
  return(htmlTable)
}

createTable3 <- function(dev, rom, tense, aspect, vowel) {
  
  # tense is either present or future for table type 2
  # aspect is indefinite, perfect, imperfect, continuous for table type 2
  
  conjugationsType3 <- c("sing_23_m", "plur_123_m", "sing_23_f", "plur_123_f")
  honaList3 <- list()
  aspectList3 <- list()
  pronounList3 <- list()
  
  for (conjugation in conjugationsType3) {
    honaList3[[conjugation]] <- conjugateHona(conjugation, tense)
    aspectList3[[conjugation]] <- conjugateAspect(conjugation, aspect, vowel)
    
    dev_string <- paste0(dev, aspectList3[[conjugation]][1], " ", honaList3[[conjugation]][1])
    rom_string <- paste0(rom, aspectList3[[conjugation]][2], " ", honaList3[[conjugation]][2])
    
    pronounList3[[conjugation]] <- paste0(dev_string, "<br>", rom_string)
  }
  
  htmlTable <- paste(
    '<table class="table table-bordered">
      <tr>
        <td colspan="3">Subject + ने; verb agrees with <b>object</b></td>
      </tr>
      <tr>
        <td style="border: none; font-weight: bold;">Object</td>
        <td><b>Singular</b></td>
        <td><b>Plural</b></td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Masc</b></td>
        <td>', pronounList3["sing_23_m"], '</td>
        <td>', pronounList3["plur_123_m"], '</td>
      </tr>
      <tr>
        <td style="vertical-align: middle;"><b>Fem</b></td>
        <td>', pronounList3["sing_23_f"], '</td>
        <td>', pronounList3["plur_123_f"], '</td>
      </tr>
    </table>'
  )
  
  return(htmlTable)
}