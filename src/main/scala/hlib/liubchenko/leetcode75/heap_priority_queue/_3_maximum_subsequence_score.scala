package hlib.liubchenko.leetcode75.heap_priority_queue

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class _3_maximum_subsequence_score extends AnyWordSpec with Matchers {
  import scala.collection.mutable

  // Not mine, runtime 100, memory 100
  def maxScore(nums1: Array[Int], nums2: Array[Int], k: Int): Long = {
    val sorted = nums1.zip(nums2).sortBy(_._2)(Ordering[Int].reverse)
    val pq = mutable.PriorityQueue.from(sorted.take(k).map(_._1.toLong)).reverse
    var sum = pq.sum
    sorted.drop(k).foldLeft(sum * sorted(k - 1)._2) { case (maxScore, (n1, n2)) =>
      sum = sum - pq.dequeue() + n1
      pq.enqueue(n1)
      math.max(maxScore, sum * n2)
    }
  }

  "maxScore" should {
    "work as expected" in {
      maxScore(Array(1, 3, 3, 2), Array(2, 1, 3, 4), 3) shouldBe 12
      maxScore(Array(4, 2, 3, 1, 1), Array(7, 5, 10, 9, 6), 1) shouldBe 30
      maxScore(Array(2, 1, 14, 12), Array(11, 7, 13, 6), 3) shouldBe 168
      maxScore(
        Array(37290, 71466, 41677, 13714, 25228, 11091, 22312, 24355, 73248, 87068, 8374, 83774, 59261, 94801, 91232,
          18702, 65803, 80975, 49986, 50233, 68942, 73208, 77016, 92414, 95912, 86725, 29657, 25259, 69114, 4261, 49875,
          79802, 56371, 88723, 41589, 80909, 11929, 45662, 85046, 67942, 62355, 46875, 50982, 37494, 9696, 15189, 7741,
          3713, 19544, 39187, 79194, 11063, 32716, 44289, 74641, 63598, 67710, 79993, 97338, 99180, 29799, 94732, 15604,
          55429, 92839, 19061, 34040, 79571, 13922, 50278, 67164, 68496, 45064, 61780, 52385, 43552, 29437, 54522,
          15824, 74461, 36917, 3994, 54918, 15386, 82026, 65593, 47192, 47314, 94201, 84712, 7210, 54335, 31721, 91130,
          89686, 22357, 90297, 12985, 25108, 2214, 5313, 17183, 64889, 19782, 76885, 56860, 4083, 68700, 99588, 57860,
          44836, 16345, 6018, 46871, 28290, 7500, 45373, 71604, 88693, 23885, 69262, 9591, 55501, 6187, 21227, 84624,
          80159, 34250, 27204, 97985, 45313, 28182, 65548, 11059, 58215, 10591, 37342, 69195, 98592, 87187, 24613,
          83279, 64270, 13135, 49426, 10189, 69197, 52113, 89652, 25410, 63317, 1762, 99955, 14916, 75718, 87471, 45355,
          99360, 52023, 28444, 8415, 98977, 27134, 7497, 46883, 1022, 78658, 34399, 55574, 852, 75518, 1170, 85762,
          62546, 3200, 95731, 51402, 42685, 22292, 16646, 4328, 34068, 69774, 53086, 39992, 4626, 48706, 86080, 3503,
          50136, 79700, 28988, 59135, 97336, 52408, 68105, 22715, 21879, 24279, 20265, 25202, 87910, 48760, 11686,
          63667, 64670, 46223, 55669, 66301, 91712, 15274, 42560, 3092, 97504, 5754, 25993, 82993, 4015, 53719, 20455,
          38020, 25083, 83192, 27317, 12386, 22888, 89639, 93235, 56058, 27412, 2849, 46942, 64030, 48288, 22465, 45481,
          37000, 23904, 86815, 65535, 84825, 54944, 36238, 45431, 37061, 95010, 83592, 10219, 88228, 46039, 9121, 19328,
          24578, 75791, 44663, 46829, 6624, 64004, 73566, 42356, 42485, 14852, 78996, 12441, 18489, 36613, 62899, 34200,
          1991, 36363, 48968, 24830, 84425, 14517, 5490, 51416, 12008, 72911, 22905, 19459, 35926, 4038, 43207, 96374,
          13196, 96436, 95243, 36998, 25673, 11082, 18676, 37353, 32836, 52758, 89608, 83700, 620, 84196, 52311, 37309,
          6799, 89038, 664, 92292, 12051, 20569, 77860, 95601, 41601, 50351, 62971, 52884, 32409, 95038, 80820, 30280,
          28194, 46761, 77647, 32444, 88956, 28683, 10734, 85116, 30752, 29156, 53561, 10658, 30763, 41443, 26890, 5268,
          7857, 31066, 73689, 37182, 35751, 11012, 14514, 54281, 11180, 46782, 56501, 37171, 50068, 94129, 77564, 64364,
          49643, 56397, 39353, 86583, 50063, 31438, 45780, 82480, 69579, 10326, 16616, 20111, 60851, 75869, 60286,
          43973, 55242, 71555, 57168, 22891, 83071, 25316, 82261, 13459, 60644, 93783, 70439, 41588, 65652, 24775,
          99592, 14532, 88990, 81309, 26785, 58011, 7576, 37078, 65191, 76106, 54307, 60494, 83161, 5365, 8510, 89797,
          31902, 38222, 42957, 95426, 76444, 25576, 2291, 92735, 66093, 31133, 45076, 57290, 26292, 99718, 79536, 13264,
          55393, 24703, 10300, 41828, 68535, 78146, 20873, 40323, 70185, 37365, 26415, 25616, 6939, 21495, 4144, 11129,
          13893, 62657, 25502, 44766, 20549, 25770, 30631, 51770, 83338, 26111, 1923, 92312, 18357, 47038, 27866, 62486,
          62277, 5098, 48607, 34438, 64415, 82025, 80276, 82880, 82484, 17318, 22685, 30544, 27724, 4630, 69609, 39858,
          81694, 37112, 16846, 14572, 74291, 21533, 44733, 93161, 16091, 86912, 72927, 76530, 68503, 66846, 53289,
          78028, 81961, 373, 2259, 21255, 54492, 33117, 79214, 78081, 31458, 27935, 78282, 1344, 74301, 14683, 25503,
          26841, 15140, 83950, 81564, 61040, 55000, 67217, 70295, 26549, 83831, 96342),
        Array(13603, 20937, 12457, 85976, 43687, 27397, 17224, 16013, 96945, 36424, 26015, 38052, 66790, 80421, 7833,
          87773, 75308, 15682, 60902, 67283, 83979, 13207, 72568, 33157, 44966, 48234, 58323, 60731, 49934, 69842, 4555,
          43513, 20520, 97097, 93463, 52172, 94105, 18548, 25244, 7967, 62807, 1602, 9528, 99688, 83331, 20950, 69744,
          80233, 95637, 21731, 57954, 93812, 72206, 78659, 21519, 15402, 2798, 97653, 53120, 16330, 2641, 20890, 25111,
          39076, 20889, 51982, 78908, 323, 68506, 65510, 77671, 21660, 41327, 32633, 68239, 94742, 28505, 54206, 42307,
          41887, 48342, 39758, 31020, 39560, 6422, 98874, 49345, 17405, 60219, 27063, 69344, 22688, 43684, 42791, 8951,
          97658, 59223, 47837, 21902, 86509, 37407, 18404, 21088, 58919, 59482, 24470, 668, 30115, 50474, 62623, 44517,
          1209, 21123, 15320, 64312, 92207, 66470, 14847, 41076, 76686, 52935, 22319, 74387, 48631, 16655, 7495, 48475,
          78218, 85915, 51092, 57862, 79637, 87809, 35387, 3929, 26793, 93984, 64903, 65490, 43888, 25180, 89243, 70039,
          25275, 42896, 36287, 14885, 40945, 67506, 88215, 61413, 73924, 3106, 63537, 36681, 33127, 42784, 95390, 56415,
          35128, 62801, 16290, 60161, 98949, 33747, 36606, 25212, 47163, 54988, 73845, 38795, 2489, 76120, 26578, 79647,
          20470, 35327, 96879, 41304, 32842, 36659, 36366, 1212, 76102, 6184, 45314, 70340, 12576, 9516, 78530, 74791,
          85595, 44240, 53241, 49054, 60505, 58421, 6918, 53981, 12947, 83470, 98224, 45483, 84529, 77484, 31696, 61827,
          76030, 49657, 19946, 79716, 55044, 76428, 2336, 15006, 42787, 30783, 89621, 39337, 80567, 36086, 37001, 88879,
          53338, 12366, 36118, 35415, 38276, 37031, 63728, 17163, 27643, 29074, 32461, 57095, 7917, 80548, 1805, 45381,
          74324, 14802, 88804, 1007, 39669, 35731, 65998, 91913, 76291, 56845, 48269, 15300, 64384, 95505, 27016, 44717,
          27595, 35140, 53955, 76692, 43936, 85074, 50967, 8347, 37258, 4482, 76323, 70584, 1415, 76739, 10488, 79790,
          83800, 60981, 58309, 85231, 34143, 17663, 59756, 97548, 30488, 35807, 97026, 88166, 85588, 86497, 22973,
          40732, 54031, 45859, 28653, 82177, 85662, 82468, 62919, 21446, 14257, 97303, 20412, 81678, 1813, 11103, 49562,
          93294, 55718, 38164, 27739, 97751, 26582, 49956, 84103, 96095, 63948, 29067, 51138, 51375, 51111, 27318,
          21470, 63767, 49093, 82787, 96131, 9224, 38068, 11500, 833, 37761, 81111, 80217, 82049, 29956, 81910, 14210,
          24516, 33164, 39954, 27770, 89254, 28454, 47835, 64348, 10841, 42147, 44748, 51502, 92408, 97421, 40351,
          94825, 42586, 13312, 682, 69860, 88091, 83925, 27647, 9743, 26357, 29470, 68175, 47774, 57600, 84798, 14904,
          32897, 11675, 89703, 71430, 91911, 4158, 66846, 72439, 89982, 71007, 15294, 39426, 2237, 81838, 16670, 83492,
          31303, 17050, 99865, 9016, 79799, 36707, 45028, 23355, 44933, 89880, 35835, 69164, 6019, 25561, 76301, 88965,
          64756, 52751, 30658, 73427, 52977, 77720, 99574, 30012, 40377, 61572, 25685, 57738, 48328, 62290, 33052,
          72512, 12029, 87392, 17710, 26135, 89542, 95488, 36858, 20714, 54795, 53719, 23173, 7841, 91813, 44845, 89900,
          63422, 55299, 70055, 93019, 95150, 74710, 14544, 69523, 59406, 69140, 69250, 32876, 39604, 4160, 73719, 35698,
          46013, 67103, 66002, 98624, 90805, 86467, 44431, 66602, 57561, 7458, 73704, 38127, 49065, 79999, 39633, 73366,
          75143, 31451, 61645, 86628, 60933, 97963, 95948, 94335, 88496, 13807, 75409, 51691, 79547, 97763, 79861,
          52403, 528, 14978, 51720, 31928, 63789, 75440, 14353, 34789, 39892, 96556, 71556, 43855, 7223, 67472, 72841,
          66787, 18446, 68168, 92862, 45925, 90010, 17885, 78480, 34456, 38318),
        116
      ) shouldBe 454087096140L
    }
  }
}
