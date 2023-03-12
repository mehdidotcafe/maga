function strStr(s1, s2) {
  let i , j = 0

  for (i = 0; i < s1.length;) {
    if (s1[i] === s2[j]) {
      ++j
    }
    ++i

    if (j === s2.length) {
      return true
    }
  }

  return false
}


function lcs_recursive(bigString, smallString, str) {
  let res = str

  if (smallString.length === 0) {
    return str
  }

    for (var i = 0; i < smallString.length; i++) {
      let fStr = str + smallString[i]


      if (strStr(bigString, fStr)) {
        let t = lcs_recursive(bigString, smallString.slice(i + 1), fStr)

        if (t.length > res.length) {
          res = t
        }
      }
    }

  return res
}

function lcs(s1, s2) {
  let smallString, bigString

  if (s1.length < s2.length) {
    smallString = s1
    bigString = s2
  } else {
    bigString = s1
    smallString = s2
  }

  return lcs_recursive(bigString, smallString, "")
}


console.log(lcs("abcjwnrfgjklwj", "umpwcajklw"))