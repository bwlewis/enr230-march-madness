library(rvest)

# idiosyncratic parser for kenpom.com

parser = function(u) {
  doc = read_html(u)
  x = html_table(html_element(doc, "table"))
  k = x[[1]] == "Rk" | x[[1]] == ""
  ans = list(rank = x[[1]][!k], team = x[[2]][!k])
  i = grep("[0-9]$", ans$team)
  ans$seed = rep(0, length(ans$team))
  ans$seed[i] = as.numeric(gsub(".* ([0-9]*)$", "\\1", ans$team[i]))
  ans$team[i] = gsub(" *[0-9]*$", "", ans$team[i])
  ans$conf = x[[3]][!k]
  ans$wins =  as.numeric(gsub("-.*", "", x[[4]][!k]))
  ans$lost =  as.numeric(gsub(".*-", "", x[[4]][!k]))
  ans$offence = as.numeric(x[[6]][!k])  # points scored per 100 possessions
  ans$defence = as.numeric(x[[8]][!k])  # points allowed per 100 possessions
  ans$tempo = as.numeric(x[[10]][!k])   # possession per 40 minutes
  ans$luck = as.numeric(x[[12]][!k])
  ans$avg_opponent_offence = as.numeric(x[[16]][!k])
  ans$avg_opponent_defence = as.numeric(x[[18]][!k])
  ans$champion = rep(0, length(ans$team))

# find champion
  tr = html_elements(doc, "tr")
  j = which(html_attr(tr, "class") == "tourney")
  if(length(j) > 0) {
    t = html_text(html_elements(tr[j], "a")[[1]])
    h = ans$team == t
    if(any(h)) ans$champion[h] = 1
  }
  as.data.frame(ans)
}

ans = Map(function(y) {
  url = paste0("https://kenpom.com/index.php?y=", y)
  ans = parser(url)
  ans$year = y
  ans
}, seq(from=2010, to=2024))
