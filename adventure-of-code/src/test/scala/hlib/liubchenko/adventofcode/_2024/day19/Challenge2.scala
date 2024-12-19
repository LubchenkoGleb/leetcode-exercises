package hlib.liubchenko.adventofcode._2024.day19

import hlib.liubchenko.adventofcode._2024.Utils
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Challenge2 extends AnyWordSpec with Matchers {
  def checkDesigns(lines: List[String]): Int = {
    val (patternsStr, designs) = lines.span(_.nonEmpty)
    val patterns = patternsStr.flatMap(_.split(", "))

    val memo = collection.mutable.Set.empty[String]

    def check(rem: String): Boolean = {
      //      println(memo.size)

      if (rem.isEmpty) true
      else if(memo.contains(rem)) false
      else patterns.filter(rem.startsWith).map(_.length).map(rem.drop).exists{p =>
        val res = check(p)
        memo.add(rem)
        res
      }
    }

    designs.tail.zipWithIndex
      .count { case (p, i) =>
        memo.clear()
        val res = check(p)
        println(s"$i: $p: $res")
        res
      }
  }

  "Day #19 Challenge #2" should {
    "work as expected #1" in {
      val input = List(
        "r, wr, b, g, bwu, rb, gb, br",
        "",
        "brwrr",
        "bggr",
        "gbbr",
        "rrbgbr",
        "ubwu",
        "bwurrg",
        "brgr",
        "bbrgwb"
      )
      checkDesigns(input) shouldBe 6
    }

    "work as expected #2" in {
      val input = List(
        "wrgrwbug, uurb, rbubr, buggb, uuwwwrb, gg, wgburr, ubg, wrbuw, grrr, rur, ggb, bur, guwggg, rwbr, grurb, bgurguw, ruub, uguugg, bwr, brggb, rwuu, rrgbuw, bgub, wbbb, rgg, ubrgb, wbrgw, rgwwuww, b, buw, grb, rww, grurw, ggwb, rbw, brbw, gb, gwwr, bugwrg, uwbww, ggu, bbbbbbur, gu, ruurw, uwr, gbur, gbgbu, wrwru, urrr, bruu, wbbgrg, gwrwr, brru, buub, wrbb, wrb, urugbur, bgwwr, bwubb, wrbbbbbb, gggbgur, www, rbuuggr, rrruwguu, wgr, r, ggw, wb, gbgr, bwbu, wgwuugug, uuug, grgw, gbbuwrb, ug, gub, wbrbug, wrrgrr, rbuu, rrg, brbu, ugwu, bguw, guww, ugw, wwg, ugrb, ggrw, uug, uww, bbw, bwrw, urr, bwbuw, rbugr, uubbwu, rru, rbb, wbb, wguu, urb, rwbrubb, rrgru, ugg, rgw, gwbb, rbrr, gbwg, wgrb, wbbbbbug, u, bru, uububwwr, wburugg, wrgugg, urgu, uuuwg, grbuwwb, brg, grwubr, wbrww, bugu, wgwrbwu, urrbr, w, wubwu, gbgugw, ggrbwr, ugurrr, ubrgu, bggrb, gugwb, rw, uuwg, wguw, wuru, rugu, ubgru, bgubr, urww, buggg, wwb, gwbu, rgbbbb, burrw, ugbuwr, uwbw, bug, urbwgwbu, gubwuur, wbgrbb, wub, bb, ugbgwr, gwbww, gggbw, bggg, gbwbur, wggg, ruuug, grw, rwg, wguwbr, rggr, wugw, rbwb, ru, wrbur, gug, rbru, buuubwb, gbwgug, ggubu, buwbwr, gru, wuw, wbg, wbr, ubgw, brwugb, rbwugr, rb, wbur, rr, wbwubuwr, wuggrb, bww, ruw, rbu, wrbrr, ubwrrwbb, guu, brb, rgrr, wgg, bgr, ggruug, bgug, ugwug, rwrrb, grbg, bbr, ugrbbrgr, gwbg, wbwgw, wbbrrb, wwgw, rwrg, wrrugr, wg, rgrwwbg, guw, urg, uugbg, wwbw, ubr, rurb, gugg, wgu, rguug, rwb, wrr, ggbwuu, gw, uuwrwu, gbbugr, rwwrwubr, uwwwgg, uur, uuu, wug, gwrrg, gwu, gwurg, bwrugr, bgb, grr, wuuug, bgw, ubwr, bwbugb, rgwrwg, wruwgru, bbu, ugbuwgu, rbr, ugrw, bugub, rwuuw, grbbuwgr, wbu, gwb, guwwurwr, gbbub, wbrg, bgrbw, urggurw, ruu, wuu, wruu, gbwb, bgwug, rurr, ubuwgw, bwugggb, gurgb, rbbwrgru, bbrb, bubu, bbur, rgb, rbrwr, bbb, bwugr, ggr, wwub, ugrwbrw, rugg, rrrwu, wbgrr, wuwrr, rgu, gbb, bwuw, wbuwrrgb, bwwrgu, ururwubr, rrbwwuw, rwrwb, bwruub, wgrrg, brr, ur, uw, bbg, wbwwr, brur, wruwbb, wwr, wwu, uuwb, ggurb, uub, rrb, rbwr, rbbbb, rg, rgubbb, ubbgruwr, wwwgu, bwgbrrw, wbw, gbguu, wgwr, grurbur, wuburuu, rwgrgr, wur, bwb, wgw, rwr, ururb, gur, rrgwww, bw, ubgbru, rbwbgrb, wrbrb, wubw, wgbub, wrg, rrw, brbr, ugu, guuuugb, wurbgwgg, uurbbgw, urw, rurg, ubw, wgbu, rwguu, gbu, ubuuw, wubruru, bwg, wuuuu, wu, wuuu, bwru, uruu, gwr, bgu, wrgr, ggrugbg, bwgu, ggg, wwwgb, rgru, wbgbu, gr, ubu, guubwu, rgwuwug, bwwru, bub, gbuu, guwbwur, gubrurw, bwrbuwr, wrw, rrrrrg, rwu, gubbuu, uru, uwbg, rurru, rrwwbb, ubgb, rgwugg, buu, ggwwbr, uwg, bbwrgu, ww, urbgbbg, uggbrww, gwg, wbrr, rbrb, ruuwgr, guubrg, uwbbb, rbg, bbgru, gbw, rwrrg, ubrwr, ugb, uubgwru, buwbuwu, wbuww, rrbr, rwbw, ggwr, grrwrg, gbrgwu, bbbu, uwub, ubb, gwur, brbruug, gww, wwugbw, rwuggw, bbrbr, rrr, rbgggw, rug, ggrrbr, brrbb, uwu, ruwwr, rgr, br, rgrubgg, ugr, wgb, wwwr, ggrbbb, uu, gubgwgb, rwbgbr, wru, wgww, rub, uuw, rwubrruu, grg, brwguug, guguu, ub, bgg, wuwu, wr, bwbg",
        "",
        "wrgrrgbbgwuwguruurwbgurgubgbwuwwwuburggbwggwrwwggbg"
      )
      checkDesigns(input) shouldBe 0
    }

    "work as expected #3" in {
      val input = Utils.readInputFile(19)
      checkDesigns(input) shouldBe 251
    }
  }
}
