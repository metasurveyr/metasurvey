// =============================================================================
// metasurvey — MongoDB Setup Script
// =============================================================================
// Usage:
//   mongosh "mongodb+srv://<user>:<password>@cluster0.xxxxx.mongodb.net/metasurvey" setup_mongodb.js
//
// Or connect first and then:
//   mongosh "mongodb+srv://..."
//   > use metasurvey
//   > load("inst/scripts/setup_mongodb.js")
// =============================================================================

print("=== metasurvey MongoDB Setup ===");
print("Database: " + db.getName());

// ---------------------------------------------------------------------------
// 1. Drop existing collections (optional — uncomment if you want a clean start)
// ---------------------------------------------------------------------------
// db.users.drop();
// db.recipes.drop();
// db.workflows.drop();

// ---------------------------------------------------------------------------
// 2. Create collections with JSON Schema validation
// ---------------------------------------------------------------------------

// --- users ---
try {
  db.createCollection("users", {
    validator: {
      $jsonSchema: {
        bsonType: "object",
        required: ["name", "email", "password_hash", "user_type", "created_at"],
        properties: {
          name:          { bsonType: "string", minLength: 1 },
          email:         { bsonType: "string", pattern: "^.+@.+\\..+$" },
          password_hash: { bsonType: "string", minLength: 64, maxLength: 64 },
          user_type:     { enum: ["individual", "institutional_member", "institution"] },
          institution:   { bsonType: ["string", "null"] },
          affiliation:   { bsonType: ["string", "null"] },
          url:           { bsonType: ["string", "null"] },
          verified:      { bsonType: "bool" },
          review_status: { enum: ["approved", "pending", "rejected"] },
          reviewed_by:   { bsonType: ["string", "null"] },
          reviewed_at:   { bsonType: ["string", "null"] },
          created_at:    { bsonType: "string" }
        }
      }
    }
  });
  print("[OK] Collection 'users' created with validation");
} catch (e) {
  if (e.codeName === "NamespaceExists") {
    // Collection exists — update the validator
    db.runCommand({
      collMod: "users",
      validator: {
        $jsonSchema: {
          bsonType: "object",
          required: ["name", "email", "password_hash", "user_type", "created_at"],
          properties: {
            name:          { bsonType: "string", minLength: 1 },
            email:         { bsonType: "string", pattern: "^.+@.+\\..+$" },
            password_hash: { bsonType: "string", minLength: 64, maxLength: 64 },
            user_type:     { enum: ["individual", "institutional_member", "institution"] },
            institution:   { bsonType: ["string", "null"] },
            affiliation:   { bsonType: ["string", "null"] },
            url:           { bsonType: ["string", "null"] },
            verified:      { bsonType: "bool" },
            created_at:    { bsonType: "string" }
          }
        }
      },
      validationLevel: "moderate"
    });
    print("[OK] Collection 'users' already exists — validator updated");
  } else {
    print("[ERROR] users: " + e.message);
  }
}

// --- recipes ---
try {
  db.createCollection("recipes", {
    validator: {
      $jsonSchema: {
        bsonType: "object",
        required: ["name", "user", "svy_type", "edition"],
        properties: {
          name:        { bsonType: "string", minLength: 1 },
          user:        { bsonType: "string" },
          svy_type:    { enum: ["ech", "eaii", "eph", "eai"] },
          edition:     { bsonType: "string" },
          description: { bsonType: "string" },
          topic:       { enum: ["labor_market", "income", "education", "health", "demographics", "housing", null] },
          doi:         { bsonType: ["string", "null"] },
          version:     { bsonType: "string" },
          downloads:   { bsonType: "int", minimum: 0 },
          steps:       { bsonType: "array", items: { bsonType: "string" } },
          depends_on:  { bsonType: "array", items: { bsonType: "string" } },
          categories: {
            bsonType: "array",
            items: {
              bsonType: "object",
              required: ["name"],
              properties: {
                name:        { bsonType: "string" },
                description: { bsonType: "string" },
                parent:      { bsonType: ["object", "null"] }
              }
            }
          },
          certification: {
            bsonType: "object",
            required: ["level"],
            properties: {
              level:        { enum: ["community", "reviewed", "official"] },
              certified_at: { bsonType: "string" },
              certified_by: {
                bsonType: ["object", "null"],
                properties: {
                  name:      { bsonType: "string" },
                  user_type: { enum: ["individual", "institutional_member", "institution"] },
                  email:     { bsonType: ["string", "null"] },
                  verified:  { bsonType: "bool" }
                }
              },
              notes: { bsonType: ["string", "null"] }
            }
          },
          user_info: {
            bsonType: ["object", "null"],
            properties: {
              name:        { bsonType: "string" },
              user_type:   { enum: ["individual", "institutional_member", "institution"] },
              email:       { bsonType: ["string", "null"] },
              affiliation: { bsonType: ["string", "null"] },
              verified:    { bsonType: "bool" },
              institution: { bsonType: ["object", "null"] }
            }
          },
          doc: {
            bsonType: ["object", "null"],
            properties: {
              input_variables:  { bsonType: "array", items: { bsonType: "string" } },
              output_variables: { bsonType: "array", items: { bsonType: "string" } },
              pipeline: {
                bsonType: "array",
                items: {
                  bsonType: "object",
                  properties: {
                    index:         { bsonType: "int" },
                    type:          { enum: ["compute", "recode", "rename", "remove", "join"] },
                    outputs:       { bsonType: ["array", "string"] },
                    inputs:        { bsonType: ["array", "string"] },
                    inferred_type: { enum: ["numeric", "categorical", "unknown", null] },
                    comment:       { bsonType: ["string", "null"] }
                  }
                }
              }
            }
          }
        }
      }
    }
  });
  print("[OK] Collection 'recipes' created with validation");
} catch (e) {
  if (e.codeName === "NamespaceExists") {
    print("[OK] Collection 'recipes' already exists — skipping validator update (use collMod manually)");
  } else {
    print("[ERROR] recipes: " + e.message);
  }
}

// --- workflows ---
try {
  db.createCollection("workflows", {
    validator: {
      $jsonSchema: {
        bsonType: "object",
        required: ["name", "user", "survey_type", "edition"],
        properties: {
          name:        { bsonType: "string", minLength: 1 },
          user:        { bsonType: "string" },
          survey_type: { enum: ["ech", "eaii", "eph", "eai"] },
          edition:     { bsonType: "string" },
          description: { bsonType: "string" },
          version:     { bsonType: "string" },
          doi:         { bsonType: ["string", "null"] },
          downloads:   { bsonType: "int", minimum: 0 },
          created_at:  { bsonType: "string" },
          estimation_type: {
            bsonType: "array",
            items: { enum: ["annual", "quarterly", "monthly"] }
          },
          recipe_ids: {
            bsonType: "array",
            items: { bsonType: "string" }
          },
          calls: {
            bsonType: "array",
            items: { bsonType: "string" }
          },
          call_metadata: {
            bsonType: "array",
            items: {
              bsonType: "object",
              required: ["type"],
              properties: {
                type:        { enum: ["svymean", "svytotal", "svyratio", "svyby"] },
                formula:     { bsonType: "string" },
                by:          { bsonType: ["string", "null"] },
                description: { bsonType: ["string", "null"] }
              }
            }
          },
          categories: {
            bsonType: "array",
            items: {
              bsonType: "object",
              required: ["name"],
              properties: {
                name:        { bsonType: "string" },
                description: { bsonType: "string" },
                parent:      { bsonType: ["object", "null"] }
              }
            }
          },
          certification: {
            bsonType: "object",
            required: ["level"],
            properties: {
              level:        { enum: ["community", "reviewed", "official"] },
              certified_at: { bsonType: "string" },
              certified_by: { bsonType: ["object", "null"] },
              notes:        { bsonType: ["string", "null"] }
            }
          },
          user_info: { bsonType: ["object", "null"] }
        }
      }
    }
  });
  print("[OK] Collection 'workflows' created with validation");
} catch (e) {
  if (e.codeName === "NamespaceExists") {
    print("[OK] Collection 'workflows' already exists — skipping validator update");
  } else {
    print("[ERROR] workflows: " + e.message);
  }
}

// ---------------------------------------------------------------------------
// 3. Create indexes
// ---------------------------------------------------------------------------
print("\n--- Creating indexes ---");

// users
db.users.createIndex({ "email": 1 }, { unique: true });
print("[OK] users.email (unique)");

// recipes
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
print("[OK] recipes indexes (7 total: id unique, user, svy_type, topic, downloads, cert level, compound, text search)");

// workflows
db.workflows.createIndex({ "id": 1 }, { unique: true, sparse: true });
db.workflows.createIndex({ "user": 1 });
db.workflows.createIndex({ "survey_type": 1 });
db.workflows.createIndex({ "recipe_ids": 1 });   // multikey index for cross-ref queries
db.workflows.createIndex({ "downloads": -1 });
db.workflows.createIndex({ "survey_type": 1, "edition": 1 });
db.workflows.createIndex(
  { "name": "text", "description": "text" },
  { name: "workflows_text_search" }
);
print("[OK] workflows indexes (7 total: id unique, user, survey_type, recipe_ids, downloads, compound, text search)");

// ---------------------------------------------------------------------------
// 4. Summary
// ---------------------------------------------------------------------------
print("\n=== Setup complete ===");
print("Collections:");
db.getCollectionNames().forEach(function(c) {
  var count = db[c].countDocuments({});
  print("  - " + c + ": " + count + " documents");
});

print("\nIndexes:");
["users", "recipes", "workflows"].forEach(function(c) {
  var indexes = db[c].getIndexes();
  print("  " + c + " (" + indexes.length + " indexes):");
  indexes.forEach(function(idx) {
    print("    - " + idx.name + ": " + JSON.stringify(idx.key));
  });
});

print("\n=== Done! ===");
print("Next steps:");
print("  1. Seed data:  mongosh <connection> inst/scripts/seed_ech_recipes.R");
print("  2. Start API:  Rscript -e 'plumber::pr_run(plumber::pr(\"inst/api/plumber.R\"), port=8787)'");
print("  3. Configure:  Sys.setenv(METASURVEY_API_URL = 'http://localhost:8787')");
