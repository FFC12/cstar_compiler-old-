# C* Falling Objects

Bu klasor, C* kodu ile yazilmis moduler bir pencere oyunu testidir.

- `modules/raylib.cstar`: Raylib C ABI import yuzeyi.
- `modules/game_types.cstar`: struct, trait, operator, input ve gameplay state kurallari.
- `modules/game_render.cstar`: render fonksiyonlari.
- `main.cstar`: frame loop, collision, score ve game-over akisi.

## Calistirma

Raylib sistemde kurulu olmalidir.

macOS:

```bash
brew install raylib
../../../build/cstar main.cstar --run
```

Linux:

```bash
sudo apt install libraylib-dev
../../../build/cstar main.cstar --run
```

Windows tarafinda Raylib library path'i compiler/linker tarafindan gorunur olmalidir.

## Kontroller

- Mouse: paddle yatay konumunu takip eder.
- `A`, `W` veya sol ok: sola hareket.
- `D` veya sag ok: saga hareket.
- Yesil objeler score arttirir.
- Kirmizi obje paddle'a degerse oyun biter.
