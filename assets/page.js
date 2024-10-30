document.addEventListener("DOMContentLoaded", (event) => {
  let opt = {weekday: "long", year: "numeric", month: "long", day: "numeric"}
  let date = new Date(document.lastModified).toLocaleDateString("en-US", opt)
  let update = document.querySelector('.update')
  if (update) {
    update.innerText += " " + date + "\n\n"
  }

  let name = document.querySelector("#name-tooltip")
  if (name) {
    name .innerHTML +=
      (`<span class="tooltiptext">
          A nickname for Cumhur, [d͡ʒumhuɾ̞̊] in Turkish IPA,
          <br>"joom hoor" in English approximation.
          <br><b>I prefer Joomy unless you speak Turkish.</b>
          <br>
          <br>📢 Listen:
          <a href="https://forvo.com/word/cumhur/">[Cumhur]</a>
          <a href="https://forvo.com/word/korkut/">[Korkut]</a>
          </span>`)
  }

  let genealogy = document.querySelector("#genealogy-tooltip")
  if (genealogy) {
    genealogy.innerHTML +=
      (`<span class="tooltiptext">
          My <a href="./others/academic-genealogy.html">academic genealogy graph</a> includes Gauss, Euler, Leibniz, Copernicus, and goes all the way back to Omar Khayyam and Avicenna!
        </span>`)
  }

  let ottoman = document.querySelector("#ottoman-tooltip")
  if (ottoman) {
    ottoman.innerHTML +=
      (`<span class="tooltiptext">
          <a href="./others/lost-in-translation.pdf">An article mentioning this work</a> appeared on the Fall 2021 edition of the Princeton International Magazine.
        </span>`)
  }

  hideAndShow()

  document.querySelectorAll(".turkce-tarih").forEach(e => {
    const d = new Date(e.innerText)
    e.innerText = d.toLocaleDateString('tr-TR', {year: 'numeric', month: 'long', day: 'numeric'})
  })
})

const hideAndShow = () => {
  let flags = document.querySelectorAll(".flag")
  if (flags.length > 0) {
    if (localStorage.tr == 1) {
      flags[1].style.fontWeight = "bold"
      flags[0].style.fontWeight = "normal"
      document.querySelectorAll(".posts-en").forEach(e => {
        e.style.display = "none"
      })
      document.querySelectorAll(".posts-tr").forEach(e => {
        e.style.display = "block"
      })
    } else {
      flags[0].style.fontWeight = "bold"
      flags[1].style.fontWeight = "normal"
      document.querySelectorAll(".posts-en").forEach(e => {
        e.style.display = "block"
      })
      document.querySelectorAll(".posts-tr").forEach(e => {
        e.style.display = "none"
      })
    }
  }
}

const enBlog = () => {
  localStorage.tr = 0
  hideAndShow()
}

const trBlog = () => {
  localStorage.tr = 1
  hideAndShow()
}
