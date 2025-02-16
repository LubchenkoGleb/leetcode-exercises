package hlib.liubchenko.topinterview150.sliding_window

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_substring_with_concatenation_of_all_words extends AnyWordSpec with Matchers {

  def findSubstring(s: String, words: Array[String]): List[Int] = {
    import scala.annotation.tailrec

    def findAllIndices(word: String, from: Int = 0): List[Int] = {
      val index = s.indexOf(word, from)
      if (index == -1) Nil else index :: findAllIndices(word, index + 1)
    }

    val mergedWords = if (words.forall(_ == words(0))) Array(words.mkString) else words

    @tailrec
    def check(from: Int, curr: Int, notUsedWords: Map[String, Int] = mergedWords.groupMapReduce(identity)(_ => 1)(_ + _)): Option[Int] =
      if (notUsedWords.isEmpty) Some(from)
      else notUsedWords.keys.find(_.zipWithIndex.forall { case (c, cI) => c == s(curr + cI) }) match {
        case Some(used) =>
          val remWords = notUsedWords.updatedWith(used) {
            case Some(1) => None
            case Some(rem) => Some(rem - 1)
            case None => None
          }
          check(from, curr + used.length, remWords)
        case None => None
      }

    val length = mergedWords.map(_.length).sum
    val candidates = mergedWords.distinct
      .flatMap(findAllIndices(_))
      .filter(_ + length <= s.length)
      .sorted
    candidates
      .flatMap { from => check(from, from) }
      .toList
  }

  "findSubstring" should {
    "work as expected" in {
      findSubstring("barfoothefoobarman", Array("foo", "bar")) should contain theSameElementsAs List(0, 9)
      findSubstring("wordgoodgoodgoodbestword", Array("word", "good", "best", "word")) shouldBe List.empty
      findSubstring("barfoofoobarthefoobarman", Array("bar", "foo", "the")) should contain theSameElementsAs
        List(6, 9, 12)
      findSubstring("foobarfoobar", Array("bar", "foo")) should contain theSameElementsAs List(0, 3, 6)
      findSubstring("aaa", Array("a", "a")) should contain theSameElementsAs List(0, 1)
      findSubstring("wordgoodgoodgoodbestword", Array("word", "good", "best", "word")) shouldBe List.empty
    }

    "work as expected with big example #1" in {
      val s = "pjzkrkevzztxductzzxmxsvwjkxpvukmfjywwetvfnujhweiybwvvsrfequzkhossmootkmyxgjgfordrpapjuunmqnxxdrqrfgkrs" +
        "jqbszgiqlcfnrpjlcwdrvbumtotzylshdvccdmsqoadfrpsvnwpizlwszrtyclhgilklydbmfhuywotjmktnwrfvizvnmfvvqfiokkdprznn" +
        "njycttprkxpuykhmpchiksyucbmtabiqkisgbhxngmhezrrqvayfsxauampdpxtafniiwfvdufhtwajrbkxtjzqjnfocdhekumttuqwovfjr" +
        "gulhekcpjszyynadxhnttgmnxkduqmmyhzfnjhducesctufqbumxbamalqudeibljgbspeotkgvddcwgxidaiqcvgwykhbysjzlzfbupkqun" +
        "uqtraxrlptivshhbihtsigtpipguhbhctcvubnhqipncyxfjebdnjyetnlnvmuxhzsdahkrscewabejifmxombiamxvauuitoltyymsarqcu" +
        "uoezcbqpdaprxmsrickwpgwpsoplhugbikbkotzrtqkscekkgwjycfnvwfgdzogjzjvpcvixnsqsxacfwndzvrwrycwxrcismdhqapoojegg" +
        "gkocyrdtkzmiekhxoppctytvphjynrhtcvxcobxbcjjivtfjiwmduhzjokkbc"
      // @formatter:off
      val words = Array("dhvf", "sind", "ffsl", "yekr", "zwzq", "kpeo", "cila", "tfty", "modg", "ztjg", "ybty", "heqg",
        "cpwo", "gdcj", "lnle", "sefg", "vimw", "bxcb")
      // @formatter:on
      findSubstring(s, words) shouldBe Nil
    }

    "work as expected with big example #2" in {
      val s = "pjzkrkevzztxductzzxmxsvwjkxpvukmfjywwetvfnujhweiybwvvsrfequzkhossmootkmyxgjgfordrpapjuunmqnxxdrqrfgkrs" +
        "jqbszgiqlcfnrpjlcwdrvbumtotzylshdvccdmsqoadfrpsvnwpizlwszrtyclhgilklydbmfhuywotjmktnwrfvizvnmfvvqfiokkdprznn" +
        "njycttprkxpuykhmpchiksyucbmtabiqkisgbhxngmhezrrqvayfsxauampdpxtafniiwfvdufhtwajrbkxtjzqjnfocdhekumttuqwovfjr" +
        "gulhekcpjszyynadxhnttgmnxkduqmmyhzfnjhducesctufqbumxbamalqudeibljgbspeotkgvddcwgxidaiqcvgwykhbysjzlzfbupkqun" +
        "uqtraxrlptivshhbihtsigtpipguhbhctcvubnhqipncyxfjebdnjyetnlnvmuxhzsdahkrscewabejifmxombiamxvauuitoltyymsarqcu" +
        "uoezcbqpdaprxmsrickwpgwpsoplhugbikbkotzrtqkscekkgwjycfnvwfgdzogjzjvpcvixnsqsxacfwndzvrwrycwxrcismdhqapoojegg" +
        "gkocyrdtkzmiekhxoppctytvphjynrhtcvxcobxbcjjivtfjiwmduhzjokkbctweqtigwfhzorjlkpuuliaipbtfldinyetoybvugevwvhhh" +
        "weejogrghllsouipabfafcxnhukcbtmxzshoyyufjhzadhrelweszbfgwpkzlwxkogyogutscvuhcllphshivnoteztpxsaoaacgxyaztuix" +
        "hunrowzljqfqrahosheukhahhbiaxqzfmmwcjxountkevsvpbzjnilwpoermxrtlfroqoclexxisrdhvfsindffslyekrzwzqkpeocilatft" +
        "ymodgztjgybtyheqgcpwogdcjlnlesefgvimwbxcbzvaibspdjnrpqtyeilkcspknyylbwndvkffmzuriilxagyerjptbgeqgebiaqnvdubr" +
        "txibhvakcyotkfonmseszhczapxdlauexehhaireihxsplgdgmxfvaevrbadbwjbdrkfbbjjkgcztkcbwagtcnrtqryuqixtzhaakjlurnum" +
        "zyovawrcjiwabuwretmdamfkxrgqgcdgbrdbnugzecbgyxxdqmisaqcyjkqrntxqmdrczxbebemcblftxplafnyoxqimkhcykwamvdsxjezk" +
        "pgdpvopddptdfbprjustquhlazkjfluxrzopqdstulybnqvyknrchbphcarknnhhovweaqawdyxsqsqahkepluypwrzjegqtdoxfgzdkydeo" +
        "xvrfhxusrujnmjzqrrlxglcmkiykldbiasnhrjbjekystzilrwkzhontwmehrfsrzfaqrbbxncphbzuuxeteshyrveamjsfiaharkcqxefgh" +
        "gceeixkdgkuboupxnwhnfigpkwnqdvzlydpidcljmflbccarbiegsmweklwngvygbqpescpeichmfidgsjmkvkofvkuehsmkkbocgejoiqcn" +
        "afvuokelwuqsgkyoekaroptuvekfvmtxtqshcwsztkrzwrpabqrrhnlerxjojemcxel"
      // @formatter:off
      val words = Array("dhvf", "sind", "ffsl", "yekr", "zwzq", "kpeo", "cila", "tfty", "modg", "ztjg", "ybty", "heqg",
        "cpwo", "gdcj", "lnle", "sefg", "vimw", "bxcb")
      // @formatter:on
      findSubstring(s, words) shouldBe List(935)
    }

    "work as expected with big example #3" in {
      val s = List.fill(10000)('a').mkString
      val words = Array.fill(5000)("a")
      findSubstring(s, words) shouldBe (0 to 5000)
    }
  }
}
