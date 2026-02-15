// =============================================================================
// metasurvey — Fix existing recipes + create indexes
// =============================================================================
// Usage:
//   mongosh "mongodb+srv://..." inst/scripts/fix_and_reindex.js
// =============================================================================

print("=== Fix & Reindex ===");

// ---------------------------------------------------------------------------
// 1. Inspect existing recipes
// ---------------------------------------------------------------------------
print("\n--- Existing recipes ---");
db.recipes.find({}).forEach(function(doc) {
  print("  _id: " + doc._id);
  print("  name: " + doc.name);
  print("  svy_type: " + JSON.stringify(doc.svy_type) + " (type: " + typeof doc.svy_type + ", isArray: " + Array.isArray(doc.svy_type) + ")");
  print("  edition: " + JSON.stringify(doc.edition) + " (type: " + typeof doc.edition + ", isArray: " + Array.isArray(doc.edition) + ")");
  print("");
});

// ---------------------------------------------------------------------------
// 2. Fix: convert array fields to strings (take first element)
// ---------------------------------------------------------------------------
print("--- Fixing array fields ---");

var fixed = 0;
db.recipes.find({}).forEach(function(doc) {
  var update = {};

  if (Array.isArray(doc.svy_type)) {
    update.svy_type = doc.svy_type[0] || "ech";
    print("  [FIX] " + doc._id + " svy_type: " + JSON.stringify(doc.svy_type) + " -> \"" + update.svy_type + "\"");
  }

  if (Array.isArray(doc.edition)) {
    update.edition = doc.edition[0] || "2023";
    print("  [FIX] " + doc._id + " edition: " + JSON.stringify(doc.edition) + " -> \"" + update.edition + "\"");
  }

  // Also fix topic if it's an array
  if (Array.isArray(doc.topic)) {
    update.topic = doc.topic[0] || null;
    print("  [FIX] " + doc._id + " topic: " + JSON.stringify(doc.topic) + " -> " + JSON.stringify(update.topic));
  }

  // Fix downloads if it's not an int
  if (doc.downloads !== undefined && typeof doc.downloads !== "number") {
    update.downloads = NumberInt(parseInt(doc.downloads) || 0);
    print("  [FIX] " + doc._id + " downloads: " + doc.downloads + " -> " + update.downloads);
  }

  // Ensure id field exists (app-level ID)
  if (!doc.id) {
    update.id = doc._id.toString();
    print("  [FIX] " + doc._id + " id: missing -> \"" + update.id + "\"");
  }

  if (Object.keys(update).length > 0) {
    db.recipes.updateOne({ _id: doc._id }, { $set: update });
    fixed++;
  }
});

if (fixed === 0) {
  print("  No fixes needed");
} else {
  print("  Fixed " + fixed + " document(s)");
}

// ---------------------------------------------------------------------------
// 3. Same fix for workflows (just in case)
// ---------------------------------------------------------------------------
print("\n--- Checking workflows ---");
db.workflows.find({}).forEach(function(doc) {
  var update = {};

  if (Array.isArray(doc.survey_type)) {
    update.survey_type = doc.survey_type[0] || "ech";
    print("  [FIX] " + doc._id + " survey_type: array -> \"" + update.survey_type + "\"");
  }

  if (Array.isArray(doc.edition) && typeof doc.edition === "object") {
    update.edition = doc.edition[0] || "2023";
    print("  [FIX] " + doc._id + " edition: array -> \"" + update.edition + "\"");
  }

  if (Object.keys(update).length > 0) {
    db.workflows.updateOne({ _id: doc._id }, { $set: update });
  }
});

// ---------------------------------------------------------------------------
// 4. Verify fix
// ---------------------------------------------------------------------------
print("\n--- Verification ---");
db.recipes.find({}).forEach(function(doc) {
  print("  " + doc._id + ": svy_type=" + JSON.stringify(doc.svy_type) + " edition=" + JSON.stringify(doc.edition));
});

// ---------------------------------------------------------------------------
// 5. Drop old indexes and recreate
// ---------------------------------------------------------------------------
print("\n--- Recreating indexes ---");

// Drop all non-_id indexes on recipes to start clean
db.recipes.getIndexes().forEach(function(idx) {
  if (idx.name !== "_id_") {
    print("  Dropping: recipes." + idx.name);
    db.recipes.dropIndex(idx.name);
  }
});

// recipes indexes
db.recipes.createIndex({ "id": 1 }, { unique: true, sparse: true });
db.recipes.createIndex({ "user": 1 });
db.recipes.createIndex({ "svy_type": 1 });
db.recipes.createIndex({ "topic": 1 });
db.recipes.createIndex({ "downloads": -1 });
db.recipes.createIndex({ "certification.level": 1 });
db.recipes.createIndex({ "svy_type": 1, "edition": 1 });
db.recipes.createIndex(
  { "name": "text", "description": "text", "topic": "text" },
  { name: "recipes_text_search" }
);
print("[OK] recipes: 8 indexes created");

// workflows — drop and recreate too
db.workflows.getIndexes().forEach(function(idx) {
  if (idx.name !== "_id_") {
    print("  Dropping: workflows." + idx.name);
    db.workflows.dropIndex(idx.name);
  }
});

db.workflows.createIndex({ "id": 1 }, { unique: true, sparse: true });
db.workflows.createIndex({ "user": 1 });
db.workflows.createIndex({ "survey_type": 1 });
db.workflows.createIndex({ "recipe_ids": 1 });
db.workflows.createIndex({ "downloads": -1 });
db.workflows.createIndex({ "survey_type": 1, "edition": 1 });
db.workflows.createIndex(
  { "name": "text", "description": "text" },
  { name: "workflows_text_search" }
);
print("[OK] workflows: 7 indexes created");

// ---------------------------------------------------------------------------
// 6. Summary
// ---------------------------------------------------------------------------
print("\n=== Final state ===");
["users", "recipes", "workflows"].forEach(function(c) {
  var count = db[c].countDocuments({});
  var indexes = db[c].getIndexes();
  print("  " + c + ": " + count + " docs, " + indexes.length + " indexes");
  indexes.forEach(function(idx) {
    print("    - " + idx.name + ": " + JSON.stringify(idx.key));
  });
});

print("\n=== Done! ===");
