
rgbasm -ogalaxia.obj galaxia.asm
rgblink -mgalaxia.map -ngalaxia.sym -ogalaxia.gbc galaxia.obj
rgbfix -v -p0 galaxia.gbc

