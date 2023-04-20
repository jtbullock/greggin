// (->> ["Electric Motor (LV)" 1]
//      get-thing
//      compress-thing
//      group-thing
//      sort-thing
//      (multiply-things 16))


// This does the recursion to build up a list of all the ingredients needed,
// and where in the process the recipe needs to be completed.

// (defn get-thing
//   "I'm not sure what to call this, actually"
//   [ [ recipeName amount ] ]
//   ( let [ ingredients (get-ingredients recipeName amount) ]
//     (if (nil? ingredients)
//       [[0 [recipeName amount]]]
//       ( let [ expanded-list ( mapcat get-thing ingredients )
//               latest-level ( apply max-key first expanded-list )
//               our-level (inc (first latest-level))]
//        (conj expanded-list [our-level [recipeName amount]])))))



// This builds the stages.

// (defn compress-thing
//   [thing]
//   (reduce
//    (fn [acc, this]
//      (if (contains? acc (first this))
//        (update-in acc [(first this)] conj (second this))
//        (assoc acc (first this) [(second this)])))
//    {}
//    thing)
// )



// This groups together the same ingredients within each stage,
// adding their quantities together, of course.

// (defn group-thing
//   [things]
//   (update-vals
//    things
//    (fn [thing]
//      (reduce
//       (fn [acc, ingredient]
//         (if (contains? acc (first ingredient))
//          (update-in acc [(first ingredient)] (fn [dictVal] (+ dictVal (second ingredient))))
//           (assoc acc (first ingredient) (second ingredient))))
//       {}
//       thing))))

// Alphabetical sort with each stage.

// (defn sort-thing
//   [things]
//   (into {} (map (fn [[level ingredients]] [level (into (sorted-map) ingredients)]) things)))


// Can be used to adjust amounts afterwards, probably not needed in the way
// this new app is set up.

// (defn multiply-things
//   [multiplier things]
//   (map (fn [[level ingredients]]
//          (let [multiplied-ingredients (map (fn [[ingredient amt]] [ingredient (* amt multiplier)]) ingredients)]
//           [level multiplied-ingredients]
//          ))
//        things)
// )