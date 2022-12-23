#lang racket

(module+ test
  (require rackunit))

;;
;; second impl of day6, should be faster
;; based on https://f4t.dev/software/go-performance-memory/
;;

;; probably more idiomatic use of "for"

;; check if a string contains duplicate chars
;; using a set
(define (has-duplicates-1? input)
    (for/fold ([seen (set)]
               #:result (not (= (set-count seen) (string-length input))))
              ([elt (in-string input)]
               #:break (set-member? seen elt))
      (set-add seen elt)))

;; variation using a byte array
(define (has-duplicates-2? s)
  ;; set an individual bit in a byte array
  (define (set-bit bs b)
    (let-values ([(i tmp) (quotient/remainder b 8)])
      (let* ([mask (arithmetic-shift 1 tmp)]
             [masked (bitwise-xor (bytes-ref bs i) mask)]
             [retval (bytes-copy bs)])
        (bytes-set! retval i masked)
        retval)))
  ;; check if a bit is set in a byte array
  (define (bit-set? bs b)
    (let-values ([(i tmp) (quotient/remainder b 8)])
      (bitwise-bit-set? (bytes-ref bs i) tmp)))

  (for/fold ([seen (make-bytes 32 0)]
             [checked 0]
             #:result (not (= checked (string-length s))))
            ([char (in-string s)]
             #:break (bit-set? seen (char->integer char)))
    (values
     (set-bit seen (char->integer char))
     (add1 checked))))

;; returns the index of the first character following a unique sequence
;; of characters in "input"
(define (index-after-prefix prefix-length input)
  (for/last ([i (range 0 (add1 (- (string-length input) prefix-length)))]
             #:final (not (has-duplicates-2? (substring input i (+ i prefix-length)))))
    (+ i prefix-length)))

(define (start-of-packet input)
  (index-after-prefix 4 input))

(module+ test
  (check-equal? 5 (start-of-packet "bvwbjplbgvbhsrlpgdmjqwftvncz"))
  (check-equal? 6 (start-of-packet "nppdvjthqldpwncqszvftbrmjlhg"))
  (check-equal? 10 (start-of-packet "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  (check-equal? 11 (start-of-packet "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

;; part 1

(define input1 "stftmtvvtvqqczqqnjnwwlqqdzdnnsvnsswbbwsstvvssfjsjbjfjmjpjzpplpppjzjqqdzzhqqqqtcccbzzzwzrrrdqdldpdsppmqmmnwwjddnqqscclncllvhllqpllchhbccfcbcgbcgcfcncsnstsddldzldlmljjfbjbzbccmrmrppqmqsswbwqwdwwcnwwhrhppfsfvsvrrfllhglhlggjpggzjgzggnvvqfvvhffpwpmwpmmwvmvrmrbmbzmzbbvgbbcfbcfbfppnzpzrrszzqgzgjgddmdwmwrmwmzznqzqhqhvvsslppsrrljjfpfcpfpbbrjjwjmjpmpfmfzfvzfftptzzbmzmddpvdddqmmzjzbbhmmwqmmmbgmmttrhrqrvqvzvdvzdvvmsvmmqlmmtddvlvttrtvtvcttvssnwwbccqmmgbbqrqlqjllmslsmslltrtffzfpfzpffvwvffsllgvgtgwtwnttfzznzqzztfzfvzznnwzzcvcqvvdwwsnsvnnthhnphpssmjmfmhfmhmgmllmsmrsrrmmhsswjwqqdbbghhpsptsswvsvfvcffqcchlhfhvhjhdjhddvjjpmmsrsqrrngrrmvmsvsllrmlrlprpggqzqmmvlvvwrwnrwwzztrzrbrdbrdbbhlhchjchhtthpthphplhhlphpjjsddtppvbpbbmnmgnmmqbbrhhfrfpfjpjgjljcctwtmwtwvvvmsvmmnjmnjmnmqmvvggtzzctzttszsvvjwjqjmjbjvbbshbhghlghhpvpnvvqmmgjmmggqvqtvqtvqtvvmlljbbhdhshnnwqwbbrnnwswmwfmfttjztjzjsjzjdjbdjjzfzdzgzhghgpgqqdzdhhnwnjnnhrnrqqjsqsllbzzzcnzzmjmvmrvrgrfgrrqmrmpmpzmzqqfwqfwfjjhphgpptzzwmmfjjvbjvbbqsbsbcbppjzpjjzpjpvjjzdjjgcgppvrvjrjpjspjppntpnnjlnlrnrdndjnnsbnnppvspprbrddjrdrmrfmmtmtbtntftjjsscvcbvvcnccnbnrbbmlmddhpdptddmrmmjddnjjtztctqqhhttqctqtbbnfnwnmndnzzljlgltgltljtjllwjjbrjbrbpbgppmzpmpggdnnmznnhhmfhhssgtgvvlsljltlppdqqbtbvbsvsswbsbhsbsmmwswbbbbhvbvcbvvsvcsvvmfmlmgmjgmmbqbrbsrrgrsggldlmlttpbpmmtptctqcqvccnhnmnssmvsmmddlclpccfwfdfqfvqfvfqqwmmwrmmbzznpnllfttgcttnbtnnsswttfppnddpdpsphhpttnrtrlrsrmrnmmzhmhnmnwwddmhdhqqhbqqdcclnlfnnzfnzzqfqddczcqcrrjsrrswslltfltffjhhqqfcchqcqpplbltbblbcbncnpccdffcwwqzwqqlwwtjjpbbjvvljvjgvvrsszfsfcssqsvvwsvvdfvdfdpffztzrtzthzhhmwmzwzssjlsldsldsldlvvjdvvnbvnvmvmccvfcvcfcbchhzqqhgqqcbbhdbdpddwcdwwzszsfszzgbgdgzgvgsgnssfqsqjssrppsgppmmpzmppglgqlqqpzpqqfzzzrnzrnrqqzzfwzzffjfzzmnznpznpprhhvcvvvfddcrdrsscnnsznzszbznntwntnrnbnzzrnnlwlggnzzwppmbmrrncnlclzltlblhhlvhvphhsdhsdsqddgzgvvshvvzzbvbrrlplqpptplljcljlvjlvlqvvndnsdddmvddzhzjjgpjpddppzllwtwfwgfgpffmhhzllsttmqqhjhhpvpgpfjsgscnwjmwmtmptwlpfjljwgpgntrlpjfgbjqmcpzgfhrwmznqnsbpptbdrzmdtvvtdqjgrjzlphndhmlchvddglqnqsjqrfqslprsvlqjwqnsmsznptsstpvdntpttslpmqqbsdlqwpjqnzmpblgqmjrvqwsncnzdszgfsghddlnwhwzpgtddgstttvrjfjwwfrgsdjjngljqlqcrzlgsmwngbzvmjwtnqdqcgwmfhsztgrtvvfzbtstmdbqpntdpsszjthqvpbdwswfzvmrcpbgbgdmldfhvdpfsmdzfhwsrpcglsztdwqgbqszcqtqjhgntzvttldqsffftzmllptzhmhpmfsgcchfrnrchnsgcfjbgrqmvrmmhnmlnwtgwhznqfgwnlrqlpjrvfrgzcjwncvlwhpclfzngbgvmrmlzngmqlvvwhbpjzlclgrcnnvnlppqhvlrnpzvmtsbdpfbwgffgzfwvltcfvfdcnfhwvcvclwwbmshhmpgrzgltwjmqczpqzdwfjpqhmwqhvvgnpgtwrjrgwvhthtdrdpnwpbmwstgblwmbfvlwflqmfbcsgwstwvncwfcsmrpcfrrvmlbqhdtdswswfnzhgzlngwsrtlzfcgdppmjnghfrgbdqhqmslhcqddjvsslsjwqqznttqjzdlghnsvqqtwrpfbzjgwnrhhvlnbqmnvcpblzgbzltnrhzpdwvbqbtmctbzgsdjfzswrbqbzgvwjlwtmgcllnmnwcljbhbplpvtgpgjftfrbgpgmhghnjcgjfqmsbqhbgtzbfzgwmfdsgfgmgzsbgdrszfhjttbvcqjzjgbqfgswlmrrhnmnfrptvjtlnvplgznsljzfzmhghlsccnqzflfnmbhshfprhclmtfptcmtnhrjgnngnqnczvcgzzlntftjsbgpgwzbnrhzzfqmznqnzfrvjzsmtpjbswzjlbgpfftzrzbfgdblpwscbqjfrfmfnhhlhjprtlzzvwnwzsnqhnmgwsdprnrblgbclzhthftqzdljspwdzwmwhfmdzmlvqsngppdfsjdprbrhffcvcvzztjjqcffwbrvpzvzfzhjvsfvsnrmjvqmjtrjbmbqsdtjgvtbbzzfmnmrrflgcdtljpmpvqvdbzgbmhjgccgdtplllctzqpfqnsztbwdbmqgfzrcddtmwrgmwsghcfpgqssdjrtqhtjfbpjvjdnzgvpzrhbrhrhcmpbglbbrvdltdpsrwjbjzftccwgnqmnlqlpjwrfdvmlgvgqznlvsmpzsmgjstvqbqpprzlsdndpfbmqcrfgvcfvlfhmpfnnqlcqlnbbgcrrrhbtzwnwmfrnrzvgmqlmqnmnzbwflwzcmncphjqztlrzvpztqhptmfsrppvvlzcfnlrwptgccsjjjscjcwnzssmbcvtzhnscgsbrchbqbrtdzllfvmqfwznfzpzmbfwcdsfhdlddnfbdgqbqjqzdtppshwcvvcjqstdgtgbhmlqlrfrhbvfsszsmbldmwfnfgnjptdslnzwcjgmvbnqcfzjmrslrlllplbhpjcnvzmvrfzwqhmbnrvpqnvcfncgfwqlvcwpwwljssfmswctcmhgtphvjdpfnnzznfbdjcsndrczlgdjhrnrltsgbtqmqcbwlthwcgsbvqbnntcznczpmlmblwdrlmzqdztwsgjthjwtfcpgwbczmdmhttzcwvdzhdfldmwnbnfdcjgvjhrfltjnjhqhzmzrlbncwdnlgshmqhpgsdwbvmvjsfgvgqzqjqdzqzmfmrncfdgrqfrnstvpqwtltnhgrmhgmmnwvlnsfmmbjrdmrnlgfgpqncdpqgvjltpghbgffdppdcfhdqhtvrdnfvcttlrqppfnqtmzpfgsnmjqfrgtbbdzzrccsrzgfjndlrnzqmjjtgldglcpzcrhwfplvdndjtzbpfbbbpfljqnlvrdzbrvczzwdrvzlmslbjsqgqdrltmhwcdpldqldlpctcbjmsvdwlfspzlpgrhdrvjtprcwrmszvzwmmjgvsbfcztmrdgrgshgtggpzszgqhwmhdzjmzsgqsfmqvcgqqwgprgvvqlbfhpwsfbjdqtcfnfhsgzthzhbpwggbnscqbnvcdprsbgllsrdcclqggfgfdrpqlqljfwpmzdhthpczcsqrrm")

(time
 (displayln "start of packet (1)")
 (start-of-packet input1))

;; part2
(define (start-of-message input)
  (index-after-prefix 14 input))

(module+ test
  (check-equal? 19 (start-of-message "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
  (check-equal? 23 (start-of-message "bvwbjplbgvbhsrlpgdmjqwftvncz"))
  (check-equal? 23 (start-of-message "nppdvjthqldpwncqszvftbrmjlhg"))
  (check-equal? 29 (start-of-message "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
  (check-equal? 26 (start-of-message "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

(time
 (displayln "start of message (2):")
 (start-of-message input1))
